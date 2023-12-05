
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

# using Decapodes
# import Decapodes: recognize_types, make_sum_mult_unique!

using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# using ..SyntacticModelsBase

#=
@intertypes "../src/amr.it" module AMR end

using .AMR



@intertypes "../src/decapodes.it" module decapodes
  import ..AMR
end

using .decapodes
=#
using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMDecapodes

h = amr.Header("", "harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
dexpr = ASKEMDecapodes.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

annot = [amr.Annotation(:X,:Form0,amr.Name("The X variable."))]

# Bundle the DecaExpr with the header metadata.
mexpr = ASKEMDecapodes.decapodes.ASKEMDecaExpr(h, dexpr, annot)



# Convert a the DecaExpr to a SummationDecapode which is the
# combinatorial representation. The converter lives in Decapodes/src/language.jl.

d = ASKEMDecapodes.SummationDecapode(mexpr.model)

# We want different metadata for this representation.
# The Summation prefix just means that this decapodes have
# specialized support for the handling of summation.
# The summation operator happens in physics so often,
# that you want to bake in some specialized handling to the data structure.

h = amr.Header("","harmonic_oscillator",
  "modelreps.io/SummationDecapode",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "SummationDecapode",
  "v1.0")
mpode = ASKEMDecapodes.decapodes.ASKEMDecapode(h, d, annot)


# The syntactic representation can be serialized as JSON.
# The resulting structure is like a parse tree of the syntactic
# representation of the DecaExpr
write_json_model(mexpr)

# We could also use the JSON serialization built into Catlab
# to serialize the resulting combinatorial representation
# sm_write_json_acset(mpode.model, "$(mpode.header.name)-acset")


 # Can we read back the models we just wrote?
@testset "Decapodes Readback" begin
  mexpr′ = readback(mexpr,ASKEMDecapodes.decapodes.ASKEMDeca)
  @test JSON3.write(mexpr) == JSON3.write(mexpr′)
end
