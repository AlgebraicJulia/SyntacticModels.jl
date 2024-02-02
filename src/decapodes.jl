module ASKEMDecapodes

export ASKEMDecaExpr, ASKEMDecapode

using ..SyntacticModelsBase
using ..AMR

using StructTypes
using DiagrammaticEquations
using DiagrammaticEquations.Deca
using MLStyle

@data ASKEMDeca <: AbstractTerm begin
  ASKEMDecaExpr(header::AMR.Header, model::DiagrammaticEquations.DecaExpr, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
  ASKEMDecapode(header::AMR.Header, model::DiagrammaticEquations.SummationDecapode, annotations::Vector{AMR.Annotation{Symbol,Symbol}})
end

@doc """    ASKEMDeca

Stores a Decapode with the model metadata for ASKEM AMR conformance.
"""
ASKEMDeca

@doc """    ASKEMDecaExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecaExpr(header::AMR.Header, model::DiagrammaticEquations.DecaExpr) = ASKEMDecaExpr(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecapode(header::AMR.Header, model::DiagrammaticEquations.SummationDecapode) = ASKEMDecapode(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

StructTypes.StructType(::Type{ASKEMDeca}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{ASKEMDeca}) = :_type
StructTypes.subtypes(::Type{ASKEMDeca}) = (ASKEMDecaExpr=ASKEMDecaExpr, ASKEMDecapode=ASKEMDecapode)

SyntacticModelsBase._dict(x::T) where {T<:Union{DiagrammaticEquations.DecaExpr, DiagrammaticEquations.Equation, DiagrammaticEquations.Term}} = begin
  Dict(:_type => typename_last(T), [k=>_dict(getfield(x, k)) for k in fieldnames(T)]...)
end

StructTypes.StructType(::Type{DiagrammaticEquations.Equation}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{DiagrammaticEquations.Equation}) = :_type
StructTypes.subtypes(::Type{DiagrammaticEquations.Equation}) = (Eq=Eq,)

StructTypes.StructType(::Type{DiagrammaticEquations.Term}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{DiagrammaticEquations.Term}) = :_type
StructTypes.subtypes(::Type{DiagrammaticEquations.Term}) = (Var=DiagrammaticEquations.Var,
  Lit=DiagrammaticEquations.Lit,
  Judgement=DiagrammaticEquations.Judgement,
  AppCirc1=DiagrammaticEquations.AppCirc1,
  App1=DiagrammaticEquations.App1,
  App2=DiagrammaticEquations.App2,
  Plus=DiagrammaticEquations.Plus,
  Mult=DiagrammaticEquations.Mult,
  Tan=DiagrammaticEquations.Tan)

end
