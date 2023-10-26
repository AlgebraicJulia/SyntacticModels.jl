export dtype_to_jsonschema, jsonwrite, jsonread, parse_dtype, toexpr, @dtype_decls, as_dtypes

using OrderedCollections
using Base64
using MLStyle
import JSON3

struct JSONFormat
end

struct DTypeConversionError <: Exception
  expected::DType
  got::Any
end

function read(format, type::Type, x)
  throw(DTypeConversionError(dtype(type), x))
end

function joinwith(io::IO, f, xs, separator)
  for x in xs[1:end-1]
    f(io, x)
    print(io, separator)
  end
  f(io, xs[end])
end

jsonwrite(x) = sprint(jsonwrite, x)
jsonwrite(io::IO, x) = write(io, JSONFormat(), x)
function jsonread(s::String, ::Type{T}) where {T}
  json = JSON3.read(s)
  read(JSONFormat(), T, json)
end

dtype(::Type{Int32}) = I32
read(::JSONFormat, ::Type{Int32}, s::Real) = Int32(s)
write(io::IO, ::JSONFormat, d::Int32) = JSON3.write(io, d)

dtype(::Type{UInt32}) = U32
read(::JSONFormat, ::Type{UInt32}, s::Real) = UInt32(s)
write(io::IO, ::JSONFormat, d::UInt32) = JSON3.write(io, d)

dtype(::Type{Int64}) = I64
read(::JSONFormat, ::Type{Int64}, s::String) = parse(Int64, s)
write(io::IO, ::JSONFormat, d::Int64) = print(io, "\"", d, "\"")

dtype(::Type{UInt64}) = U64
read(::JSONFormat, ::Type{UInt64}, s::String) = parse(UInt64, s)
write(io::IO, ::JSONFormat, d::UInt64) = print(io, "\"", d, "\"")

dtype(::Type{Float64}) = F64
read(::JSONFormat, ::Type{Float64}, s::Real) = Float64(s)
write(io::IO, ::JSONFormat, d::Float64) = JSON3.write(io, d)

dtype(::Type{Bool}) = Boolean
read(::JSONFormat, ::Type{Bool}, s::Bool) = s
write(io::IO, ::JSONFormat, d::Bool) = JSON3.write(io, d)

dtype(::Type{String}) = Str
read(::JSONFormat, ::Type{String}, s::String) = s
write(io::IO, ::JSONFormat, d::String) = JSON3.write(io, d)

dtype(::Type{Symbol}) = Sym
read(::JSONFormat, ::Type{Symbol}, s::String) = Symbol(s)
write(io::IO, ::JSONFormat, d::Symbol) = JSON3.write(io, d)

dtype(::Type{Vector{UInt8}}) = Binary
read(::JSONFormat, ::Type{Vector{UInt8}}, s::String) = base64decode(s)
function write(io::IO, ::JSONFormat, d::Vector{UInt8})
  print(io, "\"")
  Base.write(Base64EncodePipe(io), d)
  print(io, "\"")
end

dtype(::Type{Vector{T}}) where {T} = List(dtype(T))
function read(format::JSONFormat, ::Type{Vector{T}}, s::JSON3.Array) where {T}
  res = T[]
  for elt in s
    push!(res, read(format, T, elt))
  end
  res
end
function write(io::IO, format::JSONFormat, d::Vector{T}) where {T}
  print(io, "[")
  joinwith(io, (io, x) -> write(io, format, x), d, ",")
  print(io, "]")
end

dtype(::Type{OrderedDict{K,V}}) where {K,V} = Map(dtype(K), dtype(V))
function read(format::JSONFormat, ::Type{OrderedDict{K, V}}, s::JSON3.Array) where {K, V}
  res = OrderedDict{K, V}
  for elt in s
    (;key, value) = read(format, NamedTuple{(:key, :value), Tuple{K, V}}, elt)
    res[key] = value
  end
  res
end
function write(io::IO, format::JSONFormat, d::OrderedDict{K, V}) where {K, V}
  print(io, "[")
  joinwith(io, (io, x) -> write(io, format, (key=x[1], value=x[2])), d, ",")
  print(io, "]")
end

function dtype(::Type{T}) where {T<:Tuple}
  types = T.parameters
  Record(map(enumerate(types)) do (i, type)
    Field{DType}(Symbol("_", i), dtype(type))
  end)
end
function read(format::JSONFormat, ::Type{T}, s::JSON3.Object) where {T<:Tuple}
  keys = Tuple([Symbol("_", i) for i in 1:length(T.parameters)])
  Tuple(read(format, NamedTuple{keys, T}, s))
end
function write(io::IO, format::JSONFormat, d::T) where {T<:Tuple}
  keys = Tuple([Symbol("_", i) for i in 1:length(T.parameters)])
  write(io, format, NamedTuple{keys, T}(d))
end

function dtype(::Type{NamedTuple{names, T}}) where {names, T<:Tuple}
  types = T.parameters
  Record([Field{DType}(name, dtype(type)) for (name, type) in zip(names, (types))])
end
# TODO: comptime this
function read(format::JSONFormat, ::Type{NamedTuple{names, T}}, s::JSON3.Object) where {names, T<:Tuple}
  keys(s) == Set(names) || error("wrong keys: expected $names got $(keys(s))")
  vals = Any[]
  for (name, type) in zip(names, T.parameters)
    push!(vals, read(format, type, s[name]))
  end
  NamedTuple{names, T}(vals)
end
function write(io::IO, format::JSONFormat, d::NamedTuple{names, T}) where {names, T<:Tuple}
  print(io, "{")
  function writekv(io, kv::Pair{Symbol, T}) where {T}
    (k, v) = kv
    JSON3.write(io, k)
    print(io, ":")
    write(io, format, v)
  end
  joinwith(io, writekv, [pairs(d)...], ",")
  print(io, "}")
end

const Object = OrderedDict{String, Any}

function fieldproperties(fields::Vector{Field{DType}})
  map(fields) do field
    string(field.name) => dtype_to_jsonschema(field.type)
  end
end

function dtype_to_jsonschema(type::DType)
  @match type begin
    I32 => Object(
      "type" => "integer",
      "\$comment" => "I32",
      "minimum" => typemin(Int32),
      "maximum" => typemax(Int32)
    )
    U32 => Object(
      "type" => "integer",
      "\$comment" => "U32",
      "minimum" => typemin(UInt32),
      "maximum" => typemax(UInt32)
    )
    I64 => Object(
      "type" => "string",
      "\$comment" => "I64"
    )
    U64 => Object(
      "type" => "string",
      "\$comment" => "U64"
    )
    F64 => Object(
      "type" => "number",
      "\$comment" => "F64"
    )
    Boolean => Object(
      "type" => "boolean",
      "\$comment" => "Boolean"
    )
    Str => Object(
      "type" => "string",
      "\$comment" => "Str"
    )
    Binary => Object(
      "type" => "string",
      "contentEncoding" => "base64",
      "\$comment" => "Binary"
    )
    List(elemtype) => Object(
      "type" => "array",
      "items" => dtype_to_jsonschema(elemtype)
    )
    Map(keytype, valuetype) => Object(
      "type" => "array",
      "items" => Object(
        "type" => "object",
        "properties" => Object(
          "key" => dtype_to_jsonschema(keytype),
          "value" => dtype_to_jsonschema(valuetype)
        )
      )
    )
    Record(fields) => Object(
      "type" => "object",
      "properties" => Object(fieldproperties(fields)),
      "required" => string.(nameof.(fields))
    )
    Sum(variants) => Object(
      "oneOf" => Vector{Object}(map(variants) do variant
        Object(
          "type" => "object",
          "properties" => Object(
            "tag" => Object(
              "const" => string(variant.tag)
            ),
            fieldproperties(variant.fields)...
          ),
          "required" => string.(nameof.(variant.fields))
        )
      end)
    )
    Annot(desc, innertype) => begin
      innerschematype = dtype_to_jsonschema(innertype)
      innerschematype["description"] = desc
      innerschematype
    end
    TypeRef(to) => Object(
      "\$ref" => string(to)
    )
  end
end
