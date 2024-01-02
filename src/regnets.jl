using SyntacticModels
using SyntacticModels.SyntacticModelsBase
using SyntacticModels.AMR
using ACSets.ADTs
using MLStyle

@as_record Properties <: AbstractTerm
  rate_constant::String
end

@as_record struct Edge <: AbstractTerm
  id::String
  source::String
  target::String
  properties::Properties
  sign::Bool
end

@as_record struct Vertex <: AbstractTerm
  id::String
  name::String
  grounding::Any
  initial::String
  rate_constant::String
  sign::Bool
end

@data struct RegNet <: AbstractTerm
  Edgelist(vertices::Vector{Vertex}, edges::Vector{Edge})
end

@as_record struct RegNetModel <: AbstractTerm
  header::Header
  model::RegNet
  parameters::Vector{AMR.Parameter}
end
