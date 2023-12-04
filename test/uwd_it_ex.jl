using Test
using JSON3
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Catlab.Graphics

using ACSets
using ACSets.InterTypes
using Test
using OrderedCollections
import JSON

using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# using ..SyntacticModelsBase


@intertypes "../src/amr.it" module AMR end

using .AMR



@intertypes "../src/uwd.it" module uwd
  import ..AMR
end

using .uwd



# This example follows what in current catlab would be given as

#=
@relation (x:X, z:Z) where y:Y begin
  R(x,y)
  S(y,z)
  T(z,y,u)
end
=#

v1 = uwd.Typed(:x, :X)
v2 = uwd.Typed(:y, :Y)
v3 = uwd.Typed(:z, :Z)
v4 = uwd.Untyped(:u)
c = [v1, v3]
s = [uwd.Statement(:R, [v1,v2]),
  uwd.Statement(:S, [v2,v3]),
  uwd.Statement(:T, [v3,v2, v4])]
u = uwd.UWDExpr(c, s)

@testset "UWDExpr Readback" begin
  s = JSON3.write(u)
  ujson = JSON3.read(s, uwd.UWDTerm)
  # FIXME: can't compare u and ujson, because they aren't same object
  # but they have the same JSON string
  @test s == JSON3.write(ujson)
end

uwd′ = construct(RelationDiagram, u)


h = AMR.Header("","rst_relation", "modelreps.io/UWD", "A demo UWD showing generic relation composition", "UWDExpr", "v0.1")

mexpr = uwd.UWDModel(h, u)
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

to_graphviz(uwd′, box_labels=:name, junction_labels=:variable)

display(uwd′)