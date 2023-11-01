module ASKEMCollages

export ASKEMCollage, ASKEMCollageIC, ASKEMCollageBC, ASKEMCollages

using ..SyntacticModelsBase
using ..AMR

using StructTypes
using Decapodes
using MLStyle

@data ASKEMCollage <: AbstractTerm begin
  ASKEMCollageIC(header::AMR.Header, model::Decapodes.ICMorphism, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
  ASKEMCollageBC(header::AMR.Header, model::Decapodes.BCMorphism, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
  ASKEMCollages(header::AMR.Header, model::Decapodes.Collage, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
end

@doc """    ASKEMDeca

Stores a Decapode with the model metadata for ASKEM AMR conformance.
"""
ASKEMCollage

@doc """    ASKEMDecaExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollageIC(header::AMR.Header, model::Decapodes.ICMorphism) = ASKEMDecaExpr(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollageBC(header::AMR.Header, model::Decapodes.BCMorphism) = ASKEMDecapode(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMCollages(header::AMR.Header, model::Decapodes.Collage) = ASKEMCollages(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

StructTypes.StructType(::Type{ASKEMCollage}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{ASKEMCollage}) = :_type
StructTypes.subtypes(::Type{ASKEMCollage}) = (ASKEMCollageIC=ASKEMCollageIC, ASKEMCollageBC=ASKEMCollageBC, ASKEMCollages=ASKEMCollages)

#=
SyntacticModelsBase._dict(x::T) where {T<:Union{Decapodes.DecaExpr, Decapodes.Equation, Decapodes.Term}} = begin
  Dict(:_type => typename_last(T), [k=>_dict(getfield(x, k)) for k in fieldnames(T)]...)
end

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