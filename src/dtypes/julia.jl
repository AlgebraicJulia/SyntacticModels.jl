function parse_fields(fieldexprs; defined_types)
  map(enumerate(fieldexprs)) do (i, fieldexpr)
    @match fieldexpr begin
      Expr(:(::), name, type) => Field{DType}(name, parse_dtype(type; defined_types))
      Expr(:(::), type) => Field{DType}(Symbol("_", i), parse_dtype(type; defined_types))
    end
  end
end

function parse_variants(variantexprs; defined_types)
  map(variantexprs) do vexpr
    @match vexpr begin
      tag::Symbol => Variant{DType}(tag, Field{DType}[])
      Expr(:call, tag, fieldexprs...) => Variant{DType}(tag, parse_fields(fieldexprs; defined_types))
      _ => error("could not parse variant from $vexpr")
    end
  end
end

function parse_dtype(e; defined_types::AbstractSet{Symbol}=Set{Symbol}())
  @match e begin
    :Int32 => DTypes.I32
    :UInt32 => DTypes.U32
    :Int64 => DTypes.I64
    :UInt64 => DTypes.U64
    :Float64 => DTypes.F64
    :Bool => DTypes.Boolean
    :String => DTypes.Str
    :Symbol => DTypes.Sym
    :Binary => DTypes.Binary
    T::Symbol && if T ∈ defined_types end => DTypes.TypeRef(T)
    Expr(:curly, :Vector, elemtype) =>
      DTypes.List(parse_dtype(elemtype; defined_types))
    Expr(:curly, :OrderedDict, keytype, valuetype) =>
      DTypes.Map(parse_dtype(keytype; defined_types), parse_dtype(valuetype; defined_types))
    Expr(:curly, :Record, fieldexprs...) => begin
      DTypes.Record(parse_fields(fieldexprs; defined_types))
    end
    Expr(:curly, :Sum, variantexprs...) => begin
      DTypes.Sum(parse_variants(variantexprs; defined_types))
    end
    _ => error("could not parse dtype from $e")
  end
end

function parse_dtype_decl(e; defined_types::AbstractSet{Symbol}=Set{Symbol}())
  @match e begin
    Expr(:const, Expr(:(=), name::Symbol, type)) => DTypes.Alias(name, parse_dtype(type; defined_types))
    Expr(:struct, _, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      DTypes.Struct(name, parse_fields(body.args; defined_types = Set([defined_types..., name])))
    end
    Expr(:sum_type, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      DTypes.SumType(name, parse_variants(body.args; defined_types = Set([defined_types..., name])))
    end
    _ => error("could not parse dtype declaration from $e")
  end
end

function parse_dtype_decls(exprs)
  defined_types = Set{Symbol}()
  map(filter(e -> !(e isa LineNumberNode), exprs)) do expr
    decl = parse_dtype_decl(expr; defined_types)
    push!(defined_types, nameof(decl))
    decl
  end
end

module DTypeDeclImplPrivate
macro sum(head, body)
  esc(Expr(:sum_type, head, body))
end
end

macro dtype_decls(e)
  e.head == :block || error("expected a block as argument to @dtype_decls")
  e = macroexpand(DTypeDeclImplPrivate, e)
  Base.remove_linenums!(e)
  parse_dtype_decls(e.args)
end

function toexpr(field::Field)
  Expr(:(::), field.name, toexpr(field.type))
end

function toexpr(variant::Variant)
  Expr(:call, variant.tag, toexpr.(variant.fields)...)
end

function toexpr(dtype::DType)
  @match dtype begin
    I32 => :Int32
    U32 => :UInt32
    I64 => :Int64
    U64 => :UInt64
    F64 => :Float64
    Boolean => :Bool
    Str => :String
    Sym => :Symbol
    Binary => :Binary
    List(elemtype) => Expr(:curly, :Vector, toexpr(elemtype))
    Map(keytype, valuetype) => Expr(:curly, :OrderedDict, toexpr(keytype), toexpr(valuetype))
    Record(fields) =>
      Expr(:curly, :Record, toexpr.(fields)...)
    Sum(variants) =>
      Expr(:curly, :Sum, toexpr.(variants)...)
    Annot(desc, innertype) => toexpr(innertype)
    TypeRef(to) => to
  end
end

Base.show(io::IO, dtype::DType) = print(io, toexpr(dtype))

function toexpr(dtype::DTypeDecl)
  @match dtype begin
    Alias(name, type) => :(const $name = $type)
    Struct(name, fields) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@as_record")),
        nothing,
        Expr(:struct,
          false,
          name,
          Expr(:block, toexpr.(fields)...)
        )
      )
    SumType(name, variants) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@data")),
        nothing, name,
        Expr(:block, toexpr.(variants)...)
      )
  end
end

function Base.show(io::IO, decl::DTypeDecl)
  print(io, toexpr(decl))
end

as_dtypes() = as_dtypes(OrderedDict{Symbol, DType}())

function as_dtypes(context::OrderedDict{Symbol, DType})
  function parse(in::Expr)
    Base.remove_linenums!(in)
    in = macroexpand(DTypeDeclImplPrivate, in)
    decl = parse_dtype_decl(in; defined_types=keys(context))
    out = Expr(:block)
    push!(out.args, toexpr(decl))
    @match decl begin
      Alias(name, type) => begin
        context[name] = type
      end
      Struct(name, fields) => begin
        type = DTypes.Record(fields)
        context[name] = type
        push!(out.args, :($(GlobalRef(DTypes, :dtype))(::Type{$name}) = $type))
      end
      SumType(name, variants) => begin
        type = DTypes.Sum(variants)
        context[name] = type
        push!(out.args,
          :($(GlobalRef(DTypes, :dtype))(::Type{$name}) = $type)
        )
      end
    end
    push!(out.args, reader(decl))
    push!(out.args, writer(decl))
    println(out)
    out
  end
end

function variantreader(name::Symbol, fields::Vector{Field{DType}})
  fieldreads = map(fields) do field
    :($(read)(format, $(toexpr(field.type)), s[$(Expr(:quote, field.name))]))
  end
  :($name($(fieldreads...)))
end

function makeifs(branches)
  makeifs(branches[1:end-1], branches[end][2])
end

function makeifs(branches, elsebody)
  expr = elsebody
  if length(branches) == 0
    return expr
  end
  for (cond, body) in Iterators.reverse(branches[2:end])
    expr = Expr(:elseif, cond, body, expr)
  end
  (cond, body) = branches[1]
  Expr(:if, cond, body, expr)
end

function reader(decl::DTypeDecl)
  body = @match decl begin
    Alias(_, _) => nothing
    Struct(name, fields) => variantreader(name, fields)
    SumType(name, variants) => begin
      tag = gensym(:tag)
      ifs = makeifs(map(variants) do variant
        (
          :($tag == $(string(variant.tag))),
          variantreader(variant.tag, variant.fields)
        )
      end)
      quote
        $tag = s[:tag]
        $ifs
      end
    end
  end
  if !isnothing(body)
    :(function $(GlobalRef(DTypes, :read))(format::$(JSONFormat), ::Type{$(nameof(decl))}, s::$(JSON3.Object))
      $body
    end)
  else
    nothing
  end
end

function objectwriter(fields)
  fieldwriters = map(enumerate(fields)) do (i, field)
    (name, expr) = field
    quote
      print(io, "\"")
      print(io, $(string(name)))
      print(io, "\":")
      $(write)(io, format, $expr)
      $(i != length(fields) ? :(print(io, ",")) : nothing)
    end
  end
  quote
    print(io, "{")
    $(fieldwriters...)
    print(io, "}")
  end
end

function writer(decl::DTypeDecl)
  body = @match decl begin
    Alias(_, _) => nothing
    Struct(_, fields) => begin
      objectwriter([(field.name, :(d.$(field.name))) for field in fields])
    end
    SumType(name, variants) => begin
      variantlines = map(variants) do variant
        fieldnames = nameof.(variant.fields)
        fieldvars = gensym.(fieldnames)
        Expr(:call, :(=>),
          :($(variant.tag)($(fieldvars...))),
          objectwriter([(:tag, string(variant.tag)), zip(fieldnames, fieldvars)...])
        )
      end
      Expr(:macrocall, GlobalRef(MLStyle, :(var"@match")), nothing, :d,
        Expr(:block, variantlines...)
      )
    end
  end
  if !isnothing(body)
    :(function $(GlobalRef(DTypes, :write))(io::IO, format::$(JSONFormat), d::$(nameof(decl)))
      $body
    end)
  else
    nothing
  end
end
