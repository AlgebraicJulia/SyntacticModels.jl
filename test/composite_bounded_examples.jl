using ..SyntacticModels.AMR
using ..SyntacticModels.ASKEMDecapodes
using ..SyntacticModels.ASKEMUWDs
using ..SyntacticModels.ASKEMCollages
using ..SyntacticModels.BoundedComposites

using MLStyle
using JSON
using Decapodes
using Catlab
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Test



x = Typed(:X, :Form0)
v = Typed(:V, :Form0)
Q = Typed(:Q, :Form0)

c = [x, Q]
s = [Statement(:oscillator, [x,v]),
  Statement(:heating, [v,Q])]
u = ASKEMUWDs.UWDExpr(c, s)



h1 = AMR.Header("harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
d1expr = Decapodes.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

# That gave us the first model
d1 = SummationDecapode(d1expr)

bc1 = @decapode begin
  LX::Constant
  LV::Constant
end
  
  # Specify the BC morphism between Decapodes.
d1_bound_morph = BCMorphism(ACSetTransformation(bc1, d1, Var = [1,2]))
  
ic1 = @decapode begin
  (X₀, V₀)::Form0
end
      
# Specify the IC morphism between Decapodes.
d1_init_morph = ICMorphism(ACSetTransformation(ic1, d1, Var = [1,2]))
      
# Wrap these morphisms into a single collage.
d1_collage = Collage(d1_bound_morph, d1_init_morph)
      
annot1 = [AMR.Annotation(:V,:Form0,AMR.Name("The V variable."))]
      
d1c = ASKEMCollage(h1, d1_collage, annot1)



# The second model is:
h2 = AMR.Header("fricative_heating",
"modelreps.io/SummationDecapode",
"Velocity makes it get hot, but you dissipate heat away from Q₀",
"SummationDecapode", "v1.0")
d2expr = Decapodes.parse_decapode(quote
      V::Form0{Point}
      Q::Form0{Point}
      κ::Constant{Point}
      λ::Constant{Point}
      Q₀::Parameter{Point}

      ∂ₜ(Q) == κ*V + λ(Q - Q₀)
    end)
# That gave us the first model
d2 = SummationDecapode(d2expr)

bc2 = @decapode begin
  LQ::Constant
  LV::Constant
end
  
  # Specify the BC morphism between Decapodes.
d2_bound_morph = BCMorphism(ACSetTransformation(bc2, d2, Var = [2,1]))
  
ic2 = @decapode begin
  (Qinit, Vinit)::Form0
end
      
# Specify the IC morphism between Decapodes.
d2_init_morph = ICMorphism(ACSetTransformation(ic2, d2, Var = [2,1]))
      
# Wrap these morphisms into a single collage.
d2_collage = Collage(d2_bound_morph, d2_init_morph)
      
annot2 = [AMR.Annotation(:Q,:Form0,AMR.Name("The Q variable."))]
      
d2c = ASKEMCollage(h2, d2_collage, annot2)



# Now we can assemble this bad boi:
h = AMR.Header("composite_bounded_physics", "modelreps.io/BoundedComposite", "A composite bounded model", "CompositeModelExpr", "v0.0")
m = CompositeModelExpr(h, u, [OpenBoundedDecapode(d1c, [:X, :V]), OpenBoundedDecapode(d2c, [:V, :Q])])
interface(m) == [:X, :Q]
# write_json_model(m) # you can see from this little model (two coupled odes even) that the jsons will not be human editable. 

# now we can interpret this big data structure to execute a composition!
composite, bcs, ics = oapply(m)
display(apex(composite))
to_graphviz(apex(composite))
sm_write_json_acset(apex(composite),"$(m.header.name)-acset")

#=
# TESTING NESTED COMPOSITION

Q₊ = Untyped(:Q₊)
Q₋ = Untyped(:Q₋)
Q̇ = Untyped(:Q̇)

uwdʰ = UWDExpr([v, Q], [Statement(:drag, [v, Q₊]), Statement(:cooling, [Q₋, Q]), Statement(:superposition, [Q₊, Q₋, Q̇])])

drag = ASKEMDecaExpr(
  AMR.Header("DragHeat", "modelreps.io/SummationDecapode", "velocity makes it get hot", "SummationDecapode", "v1.0"),
  Decapodes.parse_decapode(quote
    V::Form0{Point}
    Q₊::Form0{Point}
    κ::Constant{Point}

    Q₊ == κ*V 
  end)
)

cooling = ASKEMDecaExpr(
  AMR.Header("NetwonCooling", "modelreps.io/SummationDecapode", "heat dissipates to the enviornment", "SummationDecapode", "v1.0"),
  Decapodes.parse_decapode(quote
    Q₋::Form0{Point}
    Q₀::Parameter{Point}
    Q::Form0{Point}
    λ::Constant{Point}

    Q₋ == λ(Q-Q₀)
  end)
)

superposition = ASKEMDecaExpr(
  AMR.Header("LinearSuperpositon", "modelreps.io/SummationDecapode", "variables be addin", "SummationDecapode", "v1.0"),
  Decapodes.parse_decapode(quote
    X::Form0{Point}
    Y::Form0{Point}
    T::Form0{Point}

    T == X + Y
  end)
)

h = AMR.Header("hierarchical_composite", "modelreps.io/Composite", "A hierarchical composite model of frictional heating", "CompositeModelExpr", "v0.1")
m = CompositeModelExpr(h,u, [OpenModel(d1, [:X, :V]),
      CompositeModelExpr(AMR.Header("heating_dynamics", "modelreps.io/Composite", "A formula for heating - cooling", "CompositeModelExpr", "v0.1"),
        uwdʰ, [OpenModel(drag, [:V, :Q₊]), OpenModel(cooling, [:Q₋, :Q]), OpenModel(superposition, [:X, :Y, :T])])
])
write_json_model(m)

@testset "Composite Model Readback" begin
  m′ = readback(m)
  @test JSON3.write(m) == JSON3.write(m′)
end

dh = apex(oapply(m))

composite = OpenDecapode(m)
hf = composite.model.header
write_json_model(ASKEMDecapode(Header("flattened_composite", hf.schema, "A flattened version of the composite_physics model.", hf.schema_name, hf.model_version), composite.model.model))
=#