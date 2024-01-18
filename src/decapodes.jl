module ASKEMDecapodes

export ASKEMDecaExpr, ASKEMDecapode

using ..SyntacticModelsBase
using ..AMR
import ..AMR: amr

using StructTypes
using Decapodes
using MLStyle
import DiagrammaticEquations: decapodes, decapodeacset

@intertypes "askemdecapodes.it" module askemdecapodes
  import ..amr
  import ..decapodes
  import ..decapodeacset
end

using .askemdecapodes
using .askemdecapodes: ASKEMDecaExpr, ASKEMDecapode

@doc """    ASKEMDeca

Stores a Decapode with the model metadata for ASKEM AMR conformance.
"""
ASKEMDeca

@doc """    ASKEMDecaExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecaExpr(header::AMR.Header, model::Decapodes.DecaExpr) = ASKEMDecaExpr(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

@doc """    ASKEMDecapode

Stores the combinatorial representation of a Decapode with the
model metadata for ASKEM AMR conformance.
"""
ASKEMDecapode(header::AMR.Header, model::Decapodes.SummationDecapode) = ASKEMDecapode(header,model,Vector{AMR.Annotation{Symbol,Symbol}}())

end
