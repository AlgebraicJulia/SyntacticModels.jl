using ..SyntacticModels
using ..SyntacticModels.SyntacticModelsBase
using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMUWDs

using Test
using JSON3
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Catlab.Graphics

using ACSets
using ACSets.InterTypes
using OrderedCollections

using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# This example follows what in current catlab would be given as

#=
@relation (x:X, z:Z) where y:Y begin
  R(x,y)
  S(y,z)
  T(z,y,u)
end
=#

v1 = ASKEMUWDs.uwd.Typed(:x, :X)
v2 = ASKEMUWDs.uwd.Typed(:y, :Y)
v3 = ASKEMUWDs.uwd.Typed(:z, :Z)
v4 = ASKEMUWDs.uwd.Untyped(:u)
c = [v1, v3]
s = [ASKEMUWDs.uwd.Statement(:R, [v1,v2]),
      ASKEMUWDs.uwd.Statement(:S, [v2,v3]),
      ASKEMUWDs.uwd.Statement(:T, [v3,v2, v4])]
u = ASKEMUWDs.uwd.UWDExpr(c, s)

@testset "UWDExpr Readback" begin
  s = jsonwrite(u)
  ujson = jsonread(s, ASKEMUWDs.uwd.UWDTerm)
  @test s == jsonwrite(ujson)
end


uwd′ = ASKEMUWDs.construct(RelationDiagram, u)


h = AMR.Header("","rst_relation", "modelreps.io/UWD", "A demo UWD showing generic relation composition", "UWDExpr", "v0.1")

mexpr = ASKEMUWDs.uwd.UWDModel(h, u)
@testset "UWD Readback" begin
  write_json_model(mexpr)
  mexpr′ = readback(mexpr,ASKEMUWDs.uwd.UWDTerm)
  
  @test mexpr.header == mexpr′.header
  @test mexpr.uwd.context == mexpr′.uwd.context
  @test mexpr.uwd.context == mexpr′.uwd.context
  @test mexpr.uwd.statements[1].relation == mexpr′.uwd.statements[1].relation
  @test mexpr.uwd.statements[1].variables == mexpr′.uwd.statements[1].variables
  @test mexpr.uwd.statements[2].relation == mexpr′.uwd.statements[2].relation
  @test mexpr.uwd.statements[2].variables == mexpr′.uwd.statements[2].variables
  @test mexpr.uwd.statements[3].relation == mexpr′.uwd.statements[3].relation
  @test mexpr.uwd.statements[3].variables == mexpr′.uwd.statements[3].variables
  
  @test all(mexpr.uwd.statements .== mexpr′.uwd.statements)
  @test mexpr == mexpr′
end

to_graphviz(uwd′, box_labels=:name, junction_labels=:variable)

display(uwd′)