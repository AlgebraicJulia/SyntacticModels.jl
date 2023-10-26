module DTypes
export DType, DTypeDecl, Binary, dtype

using MLStyle

struct Field{T}
  name::Symbol
  type::T
end

Base.nameof(field::Field) = field.name

struct Variant{T}
  tag::Symbol
  fields::Vector{Field{T}}
end

Base.nameof(variant::Variant) = variant.tag

@data DType begin
  I32
  U32
  I64
  U64
  F64
  Boolean
  Str
  Sym
  Binary
  List(elemtype::DType)
  Map(keytype::DType, valuetype::DType)
  Record(fields::Vector{Field{DType}})
  Sum(variants::Vector{Variant{DType}})
  ACSetDType(schema::BasicSchema{Symbol}, params::Vector{DType})
  Annot(desc::String, type::DType)
  TypeRef(to::Symbol)
end

@data DTypeDecl begin
  Alias(name::Symbol, type::DType)
  SumType(name::Symbol, variants::Vector{Variant{DType}})
  Struct(name::Symbol, fields::Vector{Field{DType}})
  SchemaDecl(name::Symbol, schema::BasicSchema{Symbol})
  NamedACSetType(name::Symbol, schemaname:::Symbol)
end

Base.nameof(decl::DTypeDecl) = @match decl begin
  Alias(name, _) => name
  SumType(name, _) => name
  Struct(name, _) => name
end

function dtype end

include("json.jl")
include("julia.jl")
include("python.jl")

end
