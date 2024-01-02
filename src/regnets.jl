using SyntacticModels
using SyntacticModels.SyntacticModelsBase
using SyntacticModels.AMR
import SyntacticModels.AMR: nounit
using ACSets.ADTs
using MLStyle

@as_record struct Properties <: AbstractTerm
  rate_constant::String
end

@as_record struct Edge <: AbstractTerm
  id::String
  source::String
  target::String
  sign::Bool
  properties::Properties
end

@as_record struct Vertex <: AbstractTerm
  id::String
  name::String
  grounding::Any
  initial::String
  rate_constant::String
  sign::Bool
end

@data RegNet <: AbstractTerm begin
  Edgelist(vertices::Vector{Vertex}, edges::Vector{Edge})
end

@as_record struct RegNetModel <: AbstractTerm
  header::Header
  model::RegNet
  parameters::Vector{AMR.Parameter}
end


vertices = [
  Vertex("R",
    "Rabbits",
    nothing,
    "R0",
    "alpha",
    true),
  Vertex("W",
    "Wolves",
    nothing,
    "W0",
    "gamma",
    false)
]

edges = [
  Edge(
    "wolf_eats_rabbit",
    "W",
    "R",
    false,
    Properties("beta")),
  Edge(
    "rabbit_feeds_wolf",
    "R",
    "W",
    true,
    Properties("delta"))
]

parameters = [
  Parameter(
    :R0,
    "Initial rabbit population",
    nounit,
    2.,
    PointMass(2)
  ),
  Parameter(
    :W0,
    "Initial wolf population",
    nounit,
    1.,
    Uniform(.9,1.1)
  ),
  Parameter(
    :alpha,
    "Maximum per capita prey growth rate",
    nounit,
    0.667,
    PointMass(.667)
  ),
  Parameter(
    :beta,
    "Effect of predators on prey",
    nounit,
    1.333,
    PointMass(1.333)
  ),
  Parameter(
    :gamma,
    "Effect of prey on predators",
    nounit,
    1.,
    PointMass(1)
  ),
  Parameter(
    :delta,
    "Maximum per capita predator death rate",
    nounit,
    1.,
    PointMass(1.0)
  )
]

rnm = RegNetModel(
  Header("Lotka Volterra", "https://raw.githubusercontent.com/DARPA-ASKEM/Model-Representations/regnet_v0.2/regnet/regnet_schema.json", "regnet", "Lotka Volterra model", "0.1"),
  Edgelist(vertices, edges),
  parameters
)
