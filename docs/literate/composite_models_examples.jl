# # Composing Models
#
# Now that we have learned how to specify composition patterns and primitive models, we can learn how to combine them into a composite model.
#
using SyntacticModels

using SyntacticModels.AMR
using SyntacticModels.ASKEMDecapodes
using SyntacticModels.ASKEMUWDs
using SyntacticModels.Composites

using MLStyle
import SyntacticModels.ASKEMDecapodes.Decapodes as Decapodes
using Catlab
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using DiagrammaticEquations
using Test
using JSON3

draw(uwd) = to_graphviz(uwd, box_labels=:name, junction_labels=:variable)

# ## Specifying the Composition Pattern
#
# We can build an Undirected Wiring Diagram to represent the pattern of composition.
# In this model we have 3 variables X,V,Q, which are all the same type.

x = Typed(:X, :Form0)
v = Typed(:V, :Form0)
Q = Typed(:Q, :Form0)
variables = [x,v,Q]

# Our system will expose two variables namely x and Q to the outside world.
# These variables could be used as a basis for further composition, or measured by an observer.
# The system also hase two subsystems an oscillator that couples `X` and `V` and 
# a heating element that couples `V` and `Q`.

c = [x, Q]
s = [Statement(:oscillator, [x,v]),
  Statement(:heating, [v,Q])]
u = ASKEMUWDs.UWDExpr(c, s)

# This `UWDExpr` can be interpreted with Catlab as a set of tables.
u_tables = ASKEMUWDs.construct(RelationDiagram, u)

# And visualized with graphviz as a UWD drawing.
draw(u_tables)


# ## Specifying the Component Systems
# 
# A key component of using these serialized syntactic representations is that they need to be self-describing in files.
# This is where the Header blocks come in.
h = AMR.Header("harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
# This formula is $$∂ₜ(∂ₜ(x)) = -kx$$.
dexpr = DiagrammaticEquations.parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

# That gave us the first model
d1 = ASKEMDecaExpr(h, dexpr)

# The second model is given by:
# $$ ∂ₜQ = κ*V + λ(Q - Q₀) $$
# 
#
d2 = ASKEMDecaExpr(
  AMR.Header("fricative_heating",
   "modelreps.io/SummationDecapode",
   "Velocity makes it get hot, but you dissipate heat away from Q₀",
   "SummationDecapode", "v1.0"),
    DiagrammaticEquations.parse_decapode(quote
      V::Form0{Point}
      Q::Form0{Point}
      κ::Constant{Point}
      λ::Constant{Point}
      Q₀::Parameter{Point}

      ∂ₜ(Q) == κ*V + λ(Q - Q₀)
    end)
)

# Now we can assemble this bad boi:
h = AMR.Header("composite_physics", "modelreps.io/Composite", "A composite model", "CompositeModelExpr", "v0.0")
m = CompositeModelExpr(h, u, [OpenModel(d1, [:X, :V]), OpenModel(d2, [:V, :Q])])
@test interface(m) == [:X, :Q]

# The `CompositeModelExpr` is a tree that stores the composition pattern and the models that are to be composed.
# You can see from this little model (just two coupled odes) that the json output will not be human writeable.
# This is why we need a library for Syntactic Model representations.
JSON3.pretty(m, JSON3.AlignmentContext(indent=2)) 

# We can interpret this big data structure to execute a composition!
# Notice how the variables in the composite model are namespaced with the subsystem they came from.
# The coupled variables get their names from the UWD and thus live in the top level namespace.
composite = oapply(m)
display(apex(composite))
to_graphviz(apex(composite))

# *Important: because the oapply algorithm operates on the compute graph representation of the equations, it does not produce syntactic equations. Calls to `oapply` produce instances of `OpenDecapode` and not `DecaExpr`. Software that expects to consume decapodes should plan to interact with both forms.*

# ## Nested Composition
# In this section we will build a model that is a composite of composites of models.
# This demonstrates that the Decapodes system can recursively compose multiphysics models.

Q₊ = Untyped(:Q₊)
Q₋ = Untyped(:Q₋)
Q̇ = Untyped(:Q̇)
uwdʰ = UWDExpr([v, Q], [Statement(:drag, [v, Q₊]), Statement(:cooling, [Q₋, Q]), Statement(:superposition, [Q₊, Q₋, Q̇])])

# Our new model has 3 subsystems, drag, cooling, and superposition. A key innovation of decapodes is to realize that
# even simple systems like drag are actually multiphysical, they have some term that represents a force, 
# and some term that determines how that force changes the state of the system. In order to write models compositionally,
# you must first break them down into their atomic subsystems, which are smaller than you think.
#
# Our three primitive subsystems are each composed of one equation. Of course at this scale of complexity, you don't
# need to do compositional specification, you can just compose them in your head and write down the composite.
# But this is a tutorial, so we are building a very simple model as a composite of atomic models (one equation each).
#
# The formula for drag is
# $$ Q₊ == κ*V $$
#
drag = ASKEMDecaExpr(
  AMR.Header("DragHeat", "modelreps.io/SummationDecapode", "velocity makes it get hot", "SummationDecapode", "v1.0"),
  DiagrammaticEquations.parse_decapode(quote
    V::Form0{Point}
    Q₊::Form0{Point}
    κ::Constant{Point}

    Q₊ == κ*V 
  end)
)

# Our cooling formula is
# $$ Q₋ == λ(Q-Q₀) $$

cooling = ASKEMDecaExpr(
  AMR.Header("NetwonCooling", "modelreps.io/SummationDecapode", "heat dissipates to the enviornment", "SummationDecapode", "v1.0"),
  DiagrammaticEquations.parse_decapode(quote
    Q₋::Form0{Point}
    Q₀::Parameter{Point}
    Q::Form0{Point}
    λ::Constant{Point}

    Q₋ == λ(Q-Q₀)
  end)
)

# Linear Superposition is just $$T == X + Y$$

superposition = ASKEMDecaExpr(
  AMR.Header("LinearSuperpositon", "modelreps.io/SummationDecapode", "variables be addin", "SummationDecapode", "v1.0"),
  DiagrammaticEquations.parse_decapode(quote
    X::Form0{Point}
    Y::Form0{Point}
    T::Form0{Point}

    T == X + Y
  end)
)

# The `CompositeModelExpr` type can store recursive model descriptions in terms of compositions of composite models.

h = AMR.Header("hierarchical_composite", "modelreps.io/Composite", "A hierarchical composite model of frictional heating", "CompositeModelExpr", "v0.1")
m = CompositeModelExpr(h,u, [OpenModel(d1, [:X, :V]),
      CompositeModelExpr(AMR.Header("heating_dynamics", "modelreps.io/Composite", "A formula for heating - cooling", "CompositeModelExpr", "v0.1"),
        uwdʰ, [OpenModel(drag, [:V, :Q₊]), OpenModel(cooling, [:Q₋, :Q]), OpenModel(superposition, [:X, :Y, :T])])
])

# The `oapply` function will recursively descend the tree to assemble a flat model with hierarchical namespacing. 

dh = apex(oapply(m))

# This model can also be drawn as a decapode. 

to_graphviz(dh)


# The new model description needs to be written by hand for the new model header. Some annotation in the description can't be avoided, yet.

composite = OpenDecapode(m)
hf = composite.model.header
ASKEMDecapode(Header("flattened_composite", hf.schema, "A flattened version of the composite_physics model.", hf.schema_name, hf.model_version), composite.model.model)
