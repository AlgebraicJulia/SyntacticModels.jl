
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
using Decapodes


using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# using ..SyntacticModelsBase


@intertypes "../src/amr.it" module AMR end

using .AMR



@intertypes "../src/decapodes.it" module decapodes
  import ..AMR
end

using .decapodes



h = AMR.Header("", "harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
dexpr = Decapodes.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

annot = [AMR.Annotation(:X,:Form0,AMR.Name("The X variable."))]

# Bundle the DecaExpr with the header metadata.
mexpr = decapodes.ASKEMDecaExpr(h, dexpr, annot)



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