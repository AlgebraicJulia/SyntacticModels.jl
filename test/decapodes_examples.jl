# module ASKEMDecapodesExamples

using ..SyntacticModels
using ..SyntacticModels.ASKEMDecapodes
using ..SyntacticModels.AMR

using MLStyle
using JSON
using Catlab
using ACSets
using ACSets.JSONACSets
using DiagrammaticEquations
using DiagrammaticEquations.Deca
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

annot = [AMR.Annotation(:X,:Form0,AMR.Name("The X variable."))]

# Bundle the DecaExpr with the header metadata.
mexpr = ASKEMDecaExpr(h, dexpr, annot)

# Convert a the DecaExpr to a SummationDecapode which is the
# combinatorial representation. The converter lives in DiagrammaticEquations/src/language.jl.

d = DiagrammaticEquations.SummationDecapode(mexpr.model)

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
mpode = ASKEMDecapode(h, d, annot)


# The syntactic representation can be serialized as JSON.
# The resulting structure is like a parse tree of the syntactic
# representation of the DecaExpr
write_json_model(mexpr)

# We could also use the JSON serialization built into Catlab
# to serialize the resulting combinatorial representation
sm_write_json_acset(mpode.model, "$(mpode.header.name)-acset")
# end


# Test a case in which a variable is not related to any other
h2 = AMR.Header("standalone_variable",
  "modelreps.io/DecaExpr",
  "A Test DiagrammaticEquation with a Standalone Variable",
  "DecaExpr",
  "v1.0")
dexpr2 = DiagrammaticEquations.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}
  k::Constant{Point}
  S::Form0{Point}
  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end)
annot2 = [AMR.Annotation(:X,:Form0,AMR.Name("The X variable.")),
         AMR.Annotation(:S,:Form0,AMR.Name("The standalone variable."))]
mexpr2 = ASKEMDecaExpr(h2, dexpr2, annot2)
d2 = DiagrammaticEquations.SummationDecapode(mexpr2.model)
h2 = AMR.Header("harmonic_oscillator",
  "modelreps.io/SummationDecapode",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "SummationDecapode",
  "v1.0")
mpode2 = ASKEMDecapode(h2, d2, annot2)
write_json_model(mexpr2)
sm_write_json_acset(mpode2.model, "$(mpode2.header.name)-acset")

 # Can we read back the models we just wrote?
@testset "Decapodes Readback" begin
  mexpr′ = readback(mexpr)
  @test JSON3.write(mexpr) == JSON3.write(mexpr′)
  mexpr2′ = readback(mexpr2)
  @test JSON3.write(mexpr2) == JSON3.write(mexpr2′)
end
