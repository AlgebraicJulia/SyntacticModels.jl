# # Decapodes

#=
When specifying a physical system, we use differential equations. Decapodes is a language for representing systems of equations that works with UWDs
to implement multiphysics composition. This example shows you how to use the DecaExpr language for specifying physics equations.
We only look at ordinary differential equations here, but you can think of these as partial differential equations on the zero dimensional manifold (the point).
=#

using ..SyntacticModels
using ..SyntacticModels.ASKEMDecapodes
using ..SyntacticModels.AMR

using MLStyle
using JSON3
using Catlab
using ACSets
using ACSets.JSONACSets
using DiagrammaticEquations
using Test

# Build the heder object describing the model.

h = AMR.Header("harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
dexpr = DiagrammaticEquations.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

# Bundle the DecaExpr with the header metadata.
mexpr = ASKEMDecaExpr(h, dexpr)

# Convert a the DecaExpr to a SummationDecapode which is the
# combinatorial representation. The converter lives in Decapodes/src/language.jl.

d = DiagrammaticEquations.SummationDecapode(mexpr.model)

# To visualize the Decapode as a compute graph, you can use Graphviz

to_graphviz(d)

# We want different metadata for this representation.
# The Summation prefix just means that this decapodes have
# specialized support for the handling of summation.
# The summation operator happens in physics so often,
# that you want to bake in some specialized handling to the data structure.

h = AMR.Header("harmonic_oscillator",
  "modelreps.io/SummationDecapode",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "SummationDecapode",
  "v1.0")
mpode = ASKEMDecapode(h, d)

# The syntactic representation can be serialized as JSON.
# The resulting structure is like a parse tree of the syntactic
# representation of the DecaExpr

JSON3.pretty(mexpr)

# We could also use the JSON serialization built into Catlab
# to serialize the resulting combinatorial representation

JSON3.pretty(generate_json_acset(mpode.model))
