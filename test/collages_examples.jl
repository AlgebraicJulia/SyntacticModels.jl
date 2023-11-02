# module ASKEMDecapodesExamples

using ..SyntacticModels
using ..SyntacticModels.ASKEMDecapodes
using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMCollages

using MLStyle
using JSON
using Catlab
using ACSets
using ACSets.JSONACSets
using Decapodes
using Test

# Build the heder object describing the model.

h_bc = AMR.Header("brusselator_bounded",
  "modelreps.io/ASKEMColl",
  "Example of Brusselator with boundary conditions",
  "ASKEMColl",
  "v1.0")

BrusselatorDynamics = @decapode begin
  (U, V)::Form0
  U2V::Form0
  α::Constant
  F::Parameter
  U2V == (U .* U) .* V
  ∂ₜ(U) == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
  ∂ₜ(V) == (3.4 * U) - U2V + (α * Δ(U))
end

# This is a "discrete" Decapode, with no morphisms.
BrusselatorBoundaries = @decapode begin
  L::Constant
end

# Specify the BC morphism between Decapodes.
BrusselatorBCMorphism = BCMorphism(ACSetTransformation(
  BrusselatorBoundaries, BrusselatorDynamics,
  Var = [1]))

# This is a "discrete" Decapode, with no morphisms.
BrusselatorInitialConditions = @decapode begin
  (U₀, V₀)::Form0
end

# Specify the IC morphism between Decapodes.
BrusselatorICMorphism = ICMorphism(ACSetTransformation(
  BrusselatorInitialConditions, BrusselatorDynamics,
  Var = [1,2]))

# Wrap these morphisms into a single collage.
BrusselatorCollage = Collage(
  BrusselatorBCMorphism,
  BrusselatorICMorphism)

annot = [AMR.Annotation(:U,:Form0,AMR.Name("The U variable."))]


h_ic = AMR.Header("brusselator_initial",
  "modelreps.io/ASKEMColl",
  "Example of Brusselator with initial conditions",
  "ASKEMColl",
  "v1.0")

h = AMR.Header("brusselator_collage",
  "modelreps.io/ASKEMColl",
  "Example of Brusselator with boundary and initial conditions",
  "ASKEMColl",
  "v1.0")


cbc = ASKEMCollageBC(h_bc, BrusselatorBCMorphism, annot)
cic = ASKEMCollageIC(h_ic, BrusselatorICMorphism, annot)
c = ASKEMCollage(h, BrusselatorCollage, annot)


# The syntactic representation can be serialized as JSON.
# The resulting structure is like a parse tree of the syntactic
# representation of the DecaExpr
write_json_model(cbc)
write_json_model(cic)
write_json_model(c)

# sm_write_json_acset(mpode.model, "$(mpode.header.name)-acset")

 # Can we read back the models we just wrote?
@testset "Collages Readback" begin
  cbc′ = readback(cbc)
  # read_json_acset_transformation(Decapodes.SummationDecapode{Symbol,Symbol,Symbol}(),"json/brusselator_bounded.json")
  @test JSON3.write(cbc) == JSON3.write(cbc′)
  cic′ = readback(cic)
  @test JSON3.write(cic) == JSON3.write(cic′)
  c′ = readback(c)
  @test JSON3.write(c) == JSON3.write(c′)
end