using ..SyntacticModels
using ..SyntacticModels.SyntacticModelsBase
using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMUWDs

using Test
using JSON3
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Catlab.Graphics

# This example follows what in current catlab would be given as

#=
@relation (x, z) where (x::X, y::Y, z::Z, u::U) begin
  R(x,y)
  S(y,z)
  T(z,y,u)
end
=#

v1 = Typed(:x, :X)
v2 = Typed(:y, :Y)
v3 = Typed(:z, :Z)
v4 = Untyped(:u)
c = [v1, v3]
s = [Statement(:R, [v1,v2]),
  Statement(:S, [v2,v3]),
  Statement(:T, [v3,v2, v4])]
u = UWDExpr(c, s)

@testset "UWDExpr Readback" begin
  s = JSON3.write(u)
  ujson = JSON3.read(s, UWDTerm)
  # FIXME: can't compare u and ujson, because they aren't same object
  # but they have the same JSON string
  @test s == JSON3.write(ujson)
end

uwd = ASKEMUWDs.construct(RelationDiagram, u)

h = AMR.Header("rst_relation", "modelreps.io/UWD", "A demo UWD showing generic relation composition", "UWDExpr", "v0.1")

mexpr = UWDModel(h, u)
write_json_model(mexpr)
mexpr′ = readback(mexpr)
@testset "UWD Readback" begin
  @test mexpr.header == mexpr′.header
  @test mexpr.uwd.context == mexpr′.uwd.context
  @test mexpr.uwd.context == mexpr′.uwd.context
  @test mexpr.uwd.statements[1].relation == mexpr′.uwd.statements[1].relation
  @test mexpr.uwd.statements[1].variables == mexpr′.uwd.statements[1].variables
  @test mexpr.uwd.statements[2].relation == mexpr′.uwd.statements[2].relation
  @test mexpr.uwd.statements[2].variables == mexpr′.uwd.statements[2].variables
  @test mexpr.uwd.statements[3].relation == mexpr′.uwd.statements[3].relation
  @test mexpr.uwd.statements[3].variables == mexpr′.uwd.statements[3].variables
  # TODO: overload == for statements
  # @test all(mexpr.uwd.statements .== mexpr′.uwd.statements)
  # @test mexpr == mexpr′
end

@testset "UWD show" begin
  @test sprint(show, u) == 
    "{ R(x:X, y:Y)\n  S(y:Y, z:Z)\n  T(z:Z, y:Y, u) } where {x:X, z:Z}"
  @test sprint(show, JSON3.read(JSON3.write(u), UWDTerm)) == 
    "{ R(x:X, y:Y)\n  S(y:Y, z:Z)\n  T(z:Z, y:Y, u) } where {x:X, z:Z}"
  @test sprint(show, mexpr) ==
    "\"\"\"\nASKE Model Representation: rst_relationv0.1 :: UWDExpr \n   modelreps.io/UWD\n\nA demo UWD showing generic relation composition\n\"\"\"\nUWD:\n{ R(x:X, y:Y)\n  S(y:Y, z:Z)\n  T(z:Z, y:Y, u) } where {x:X, z:Z}"
end

to_graphviz(uwd, box_labels=:name, junction_labels=:variable)

display(uwd)
