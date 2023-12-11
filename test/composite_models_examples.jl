using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMDecapodes
using ..SyntacticModels.ASKEMUWDs
using ..SyntacticModels.Composites

# using Decapodes
using Catlab
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Test
import JSON3


x = Typed(:X, :Form0)
v = Typed(:V, :Form0)
Q = Typed(:Q, :Form0)

c = [x, Q]
s = [ASKEMUWDs.uwd.Statement(:oscillator, [x,v]),
  ASKEMUWDs.uwd.Statement(:heating, [v,Q])]
u = UWDExpr(c, s)



h = Header("harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
dexpr = parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

# That gave us the first model
d1 = ASKEMDecaExpr(h, dexpr, [])

# The second model is:
d2 = ASKEMDecaExpr(
  Header("fricative_heating",
   "modelreps.io/SummationDecapode",
   "Velocity makes it get hot, but you dissipate heat away from Q₀",
   "SummationDecapode", "v1.0"),
    parse_decapode(quote
      V::Form0{Point}
      Q::Form0{Point}
      κ::Constant{Point}
      λ::Constant{Point}
      Q₀::Parameter{Point}

      ∂ₜ(Q) == κ*V + λ(Q - Q₀)
    end),
    []
)

# Now we can assemble this bad boi:
h = Header("composite_physics", "modelreps.io/Composite", "A composite model", "CompositeModelExpr", "v0.0")
m = CompositeModelExpr(h, u, [OpenModel(d1, [:X, :V]), OpenModel(d2, [:V, :Q])])
interface(m) == [:X, :Q]
#= TODO: FIXME 
write_json_model(m) # you can see from this little model (two coupled odes even) that the jsons will not be human editable. 
=#

# now we can interpret this big data structure to execute a composition!
composite = oapply(m)
display(apex(composite))
to_graphviz(apex(composite))
#= TODO: FIXME 
sm_write_json_acset(apex(composite),"$(m.header.name)-acset")
=#

# TESTING NESTED COMPOSITION

Q₊ = Untyped(:Q₊)
Q₋ = Untyped(:Q₋)
Q̇ = Untyped(:Q̇)

uwdʰ = UWDExpr([v, Q], [ASKEMUWDs.uwd.Statement(:drag, [v, Q₊]), ASKEMUWDs.uwd.Statement(:cooling, [Q₋, Q]), ASKEMUWDs.uwd.Statement(:superposition, [Q₊, Q₋, Q̇])])

drag = ASKEMDecaExpr(
  Header("DragHeat", "modelreps.io/SummationDecapode", "velocity makes it get hot", "SummationDecapode", "v1.0"),
  parse_decapode(quote
    V::Form0{Point}
    Q₊::Form0{Point}
    κ::Constant{Point}

    Q₊ == κ*V 
  end), []
)

cooling = ASKEMDecaExpr(
  Header("NetwonCooling", "modelreps.io/SummationDecapode", "heat dissipates to the enviornment", "SummationDecapode", "v1.0"),
  parse_decapode(quote
    Q₋::Form0{Point}
    Q₀::Parameter{Point}
    Q::Form0{Point}
    λ::Constant{Point}

    Q₋ == λ(Q-Q₀)
  end), []
)

superposition = ASKEMDecaExpr(
  Header("LinearSuperpositon", "modelreps.io/SummationDecapode", "variables be addin", "SummationDecapode", "v1.0"),
  parse_decapode(quote
    X::Form0{Point}
    Y::Form0{Point}
    T::Form0{Point}

    T == X + Y
  end), []
)

h = Header("hierarchical_composite", "modelreps.io/Composite", "A hierarchical composite model of frictional heating", "CompositeModelExpr", "v0.1")
m = CompositeModelExpr(h,u, [OpenModel(d1, [:X, :V]),
      CompositeModelExpr(Header("heating_dynamics", "modelreps.io/Composite", "A formula for heating - cooling", "CompositeModelExpr", "v0.1"),
        uwdʰ, [OpenModel(drag, [:V, :Q₊]), OpenModel(cooling, [:Q₋, :Q]), OpenModel(superposition, [:X, :Y, :T])])
])
#= TODO: FIXME
write_json_model(m)

@testset "Composite Model Readback" begin
  m′ = readback(m)
  @test JSON3.write(m) == JSON3.write(m′)
end
=#
dh = apex(oapply(m))

composite = OpenDecapode(m)
hf = composite.model.header
#= TODO: FIXME
write_json_model(ASKEMDecapode(Header("flattened_composite", hf.schema, "A flattened version of the composite_physics model.", hf.schema_name, hf.model_version), composite.model.model))
=#