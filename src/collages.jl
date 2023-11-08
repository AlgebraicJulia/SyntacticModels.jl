module ASKEMCollages

export ASKEMColl, ASKEMCollageIC, ASKEMCollageBC, ASKEMCollage

using ..SyntacticModelsBase
using ..AMR

using StructTypes
using Decapodes
using MLStyle

@data ASKEMColl <: AbstractTerm begin
  ASKEMCollageIC(header::AMR.Header, model::Decapodes.ICMorphism, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
  ASKEMCollageBC(header::AMR.Header, model::Decapodes.BCMorphism, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
  ASKEMCollage(header::AMR.Header, model::Decapodes.Collage, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
end

@doc """    ASKEMDeca

Stores a Decapode with the model metadata for ASKEM AMR conformance.
"""
ASKEMColl

@doc """    ASKEMDecaExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollageIC(header::AMR.Header, model::Decapodes.ICMorphism) = ASKEMCollageIC(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollageBC(header::AMR.Header, model::Decapodes.BCMorphism) = ASKEMCollageBC(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollage(header::AMR.Header, model::Decapodes.Collage) = ASKEMCollages(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

StructTypes.StructType(::Type{ASKEMColl}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{ASKEMColl}) = :_type
StructTypes.subtypes(::Type{ASKEMColl}) = (ASKEMCollageIC=ASKEMCollageIC, ASKEMCollageBC=ASKEMCollageBC, ASKEMCollage=ASKEMCollage)
SyntacticModelsBase._dict(x::T) where {T<:Union{Decapodes.ICMorphism, Decapodes.BCMorphism, Decapodes.Collage}} = begin
  Dict(:_type => typename_last(T), [k=>_dict(getfield(x, k)) for k in fieldnames(T)]...)
end
#=
StructTypes.StructType(::Type{Decapodes.Equation}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{Decapodes.Equation}) = :_type
StructTypes.subtypes(::Type{Decapodes.Equation}) = (Eq=Eq,)

StructTypes.StructType(::Type{Decapodes.Term}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{Decapodes.Term}) = :_type
StructTypes.subtypes(::Type{Decapodes.Term}) = (Var=Decapodes.Var,
  Lit=Decapodes.Lit,
  Judgement=Decapodes.Judgement,
  AppCirc1=Decapodes.AppCirc1,
  App1=Decapodes.App1,
  App2=Decapodes.App2,
  Plus=Decapodes.Plus,
  Mult=Decapodes.Mult,
  Tan=Decapodes.Tan)
=#

end