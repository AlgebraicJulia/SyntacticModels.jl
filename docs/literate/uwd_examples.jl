# # Undirected Wiring Diagrams

# To specify complex systems, you need to specify primitive models and a pattern of composition.
# This example shows you how to use Undirected Wiring Diagrams (UWDs) as a language for expressing patterns of composition.
# These diagrams are undirected, because they do not have inputs and outputs. UWDs are for systems that compose by sharing variables.
# They are not for systems that compose like functions, where output of a system is passed as input to another system.
# For systems that compose like functions, use Directed Wiring Diagrams.

using ..SyntacticModels
using ..SyntacticModels.SyntacticModelsBase
using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMUWDs

using Test
using JSON3
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Catlab.Graphics

draw(uwd) = to_graphviz(uwd, box_labels=:name, junction_labels=:variable)

#=
This example follows what in current catlab would be given as:

```julia
@relation (x:X, z:Z) where y:Y begin
  R(x,y)
  S(y,z)
  T(z,y,u)
end
```

Eventually, we will update the `@relation` macro to use this ADT based representation.
This will allow users to create syntactic UWDExprs from with an easy to write syntax embedded in Julia.
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

# We can test that if we write the `UWDExpr` into a JSON string, then we get the same information when we read it.
# The `==` operator for MLStyle types is not correctly working for these types, I think because some type information is being lost.

s = JSON3.write(u)
ujson = JSON3.read(s, UWDTerm)
@test s == JSON3.write(ujson)

# The element type of the array changes when you go through JSON, even though type of the elements are the same.
typeof(ujson.statements), typeof(u.statements)

# One can construct a Catlab Relation diagram from this expression.
uwd = ASKEMUWDs.construct(RelationDiagram, u)

# And then use Graphviz to draw the uwd.
draw(uwd)

display(uwd)

# ## Model Headers

# As usual, we can add an AMR header to a UWD Model:

h = AMR.Header("rst_relation", "modelreps.io/UWD", "A demo UWD showing generic relation composition", "UWDExpr", "v0.1")
mexpr = UWDModel(h, u)
s = JSON3.write(mexpr)

# Just as check, the headers should be preserved.
@test JSON3.write(JSON3.read(s, UWDModel)) == JSON3.write(mexpr)