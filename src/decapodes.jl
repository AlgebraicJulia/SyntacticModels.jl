module ASKEMDecapodes

export ASKEMDecaExpr, ASKEMDecapode

using ..SyntacticModels: AbstractTerm
using ..AMR

using Decapodes
using MLStyle

@data ASKEMDeca <: AbstractTerm begin
  ASKEMDecaExpr(header::AMR.Header, model::Decapodes.DecaExpr)
  ASKEMDecapode(header::AMR.Header, model::Decapodes.SummationDecapode)
end

@doc """    ASKEMDeca

Stores a Decapode with the model metadata for ASKEM AMR conformance.
"""
ASKEMDeca

@doc """    ASKEMDecaExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecaExpr

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecapode

end
