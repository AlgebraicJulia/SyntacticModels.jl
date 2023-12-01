
# using SyntacticModels

using ACSets
using ACSets.InterTypes
using Test
using OrderedCollections
import JSON
import JSON3
# import JSONSchema

# include("src/SyntacticModels.jl")

# using .SyntacticModels
# # using .SyntacticModels.ASKEMDecapodes



using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# using ..SyntacticModelsBase


@intertypes "../src/amr.it" module amr end

using .amr



@intertypes "../src/decapodes.it" module decapodes
  import ..amr
end

using .decapodes



#=
@intertypes "simpleast.it" module simpleast end

using .simpleast

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

s = jsonwrite(t)

@test s isa String

@test jsonread(s, Term) == t

generate_jsonschema_module(simpleast, ".")

simpleast_schema = JSONSchema.Schema(read("simpleast_schema.json", String))

@test JSONSchema._validate(simpleast_schema, JSON.parse(s), "Term") === nothing

@intertypes "model.it" module model
  import ..simpleast
end

using .model

e = Equation(t, t)

m = Model([:x], [e])

@test testjson(m)

@intertypes "wgraph.it" module wgraph end

using .wgraph

g = EDWeightedGraph()
add_parts!(g, :V, 2)
add_part!(g, :E, src=1, tgt=2, weight=EdgeData(:mass_ave, 42))

@test testjson(m)

generate_jsonschema_module(wgraph, ".")

wgraph_schema = JSONSchema.Schema(read("wgraph_schema.json", String))

@test JSONSchema._validate(wgraph_schema, JSON.parse(jsonwrite(g)), "EDWeightedGraph") === nothing
=#