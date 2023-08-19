var documenterSearchIndex = {"docs":
[{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"EditURL = \"../../literate/literate_example.jl\"","category":"page"},{"location":"generated/literate_example/#Composing-Models","page":"Composing Models","title":"Composing Models","text":"","category":"section"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"This is an example of adding a code example compiled with Literate.jl in the docs.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"First we want to load our package with using","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"using SyntacticModels\n\nusing SyntacticModels.AMR\nusing SyntacticModels.ASKEMDecapodes\nusing SyntacticModels.ASKEMUWDs\nusing SyntacticModels.Composites\n\nusing MLStyle\nimport SyntacticModels.ASKEMDecapodes.Decapodes as Decapodes\nusing Catlab\nusing Catlab.RelationalPrograms\nusing Catlab.WiringDiagrams\nusing Test\nusing JSON3\n\ndraw(uwd) = to_graphviz(uwd, box_labels=:name, junction_labels=:variable)","category":"page"},{"location":"generated/literate_example/#Specifying-the-Composition-Pattern","page":"Composing Models","title":"Specifying the Composition Pattern","text":"","category":"section"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"We can build an Undirected Wiring Diagram to represent the pattern of composition. In this model we have 3 variables X,V,Q, which are all the same type.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"x = Typed(:X, :Form0)\nv = Typed(:V, :Form0)\nQ = Typed(:Q, :Form0)\nvariables = [x,v,Q]","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Our system will expose two variables namely x and Q to the outside world. These variables could be used as a basis for further composition, or measured by an observer. The system also hase two subsystems an oscillator that couples X and V and a heating element that couples V and Q.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"c = [x, Q]\ns = [Statement(:oscillator, [x,v]),\n  Statement(:heating, [v,Q])]\nu = ASKEMUWDs.UWDExpr(c, s)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"This UWDExpr can be interpreted with Catlab as a set of tables.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"u_tables = ASKEMUWDs.construct(RelationDiagram, u)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"And visualized with graphviz as a UWD drawing.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"draw(u_tables)","category":"page"},{"location":"generated/literate_example/#Specifying-the-Component-Systems","page":"Composing Models","title":"Specifying the Component Systems","text":"","category":"section"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"A key component of using these serialized syntactic representations is that they need to be self-describing in files. This is where the Header blocks come in.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"h = AMR.Header(\"harmonic_oscillator\",\n  \"modelreps.io/DecaExpr\",\n  \"A Simple Harmonic Oscillator as a Diagrammatic Equation\",\n  \"DecaExpr\",\n  \"v1.0\")","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The easiest way to write down a DecaExpr is in our DSL and calling the parser.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"dexpr = Decapodes.parse_decapode(quote\n  X::Form0{Point}\n  V::Form0{Point}\n\n  k::Constant{Point}\n\n  ∂ₜ(X) == V\n  ∂ₜ(V) == -1*k*(X)\nend\n)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"That gave us the first model","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"d1 = ASKEMDecaExpr(h, dexpr)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The second model is:","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"d2 = ASKEMDecaExpr(\n  AMR.Header(\"fricative_heating\",\n   \"modelreps.io/SummationDecapode\",\n   \"Velocity makes it get hot, but you dissipate heat away from Q₀\",\n   \"SummationDecapode\", \"v1.0\"),\n    Decapodes.parse_decapode(quote\n      V::Form0{Point}\n      Q::Form0{Point}\n      κ::Constant{Point}\n      λ::Constant{Point}\n      Q₀::Parameter{Point}\n\n      ∂ₜ(Q) == κ*V + λ(Q - Q₀)\n    end)\n)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Now we can assemble this bad boi:","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"h = AMR.Header(\"composite_physics\", \"modelreps.io/Composite\", \"A composite model\", \"CompositeModelExpr\", \"v0.0\")\nm = CompositeModelExpr(h, u, [OpenModel(d1, [:X, :V]), OpenModel(d2, [:V, :Q])])\n@test interface(m) == [:X, :Q]","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The CompositeModelExpr is a tree that stores the composition pattern and the models that are to be composed. You can see from this little model (just two coupled odes) that the json output will not be human writeable. This is why we need a library for Syntactic Model representations.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"JSON3.pretty(m, JSON3.AlignmentContext(indent=2))","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"We can interpret this big data structure to execute a composition! Notice how the variables in the composite model are namespaced with the subsystem they came from. The coupled variables get their names from the UWD and thus live in the top level namespace.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"composite = oapply(m)\ndisplay(apex(composite))\nto_graphviz(apex(composite))","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Important: because the oapply algorithm operates on the compute graph representation of the equations, it does not produce syntactic equations. Calls to oapply produce instances of OpenDecapode and not DecaExpr. Software that expects to consume decapodes should plan to interact with both forms.","category":"page"},{"location":"generated/literate_example/#Nested-Composition","page":"Composing Models","title":"Nested Composition","text":"","category":"section"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"In this section we will build a model that is a composite of composites of models. This demonstrates that the Decapodes system can recursively compose multiphysics models.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Q₊ = Untyped(:Q₊)\nQ₋ = Untyped(:Q₋)\nQ̇ = Untyped(:Q̇)\nuwdʰ = UWDExpr([v, Q], [Statement(:drag, [v, Q₊]), Statement(:cooling, [Q₋, Q]), Statement(:superposition, [Q₊, Q₋, Q̇])])","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Our new model has 3 subsystems, drag, cooling, and superposition. A key innovation of decapodes is to realize that even simple systems like drag are actually multiphysical, they have some term that represents a force, and some term that determines how that force changes the state of the system. In order to write models compositionally, you must first break them down into their atomic subsystems, which are smaller than you think.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"Our three primitive subsystems are each composed of one equation. Of course at this scale of complexity, you don't need to do compositional specification, you can just compose them in your head and write down the composite. But this is a tutorial, so we are building a very simple model as a composite of atomic models (one equation each).","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"drag = ASKEMDecaExpr(\n  AMR.Header(\"DragHeat\", \"modelreps.io/SummationDecapode\", \"velocity makes it get hot\", \"SummationDecapode\", \"v1.0\"),\n  Decapodes.parse_decapode(quote\n    V::Form0{Point}\n    Q₊::Form0{Point}\n    κ::Constant{Point}\n\n    Q₊ == κ*V\n  end)\n)\n\ncooling = ASKEMDecaExpr(\n  AMR.Header(\"NetwonCooling\", \"modelreps.io/SummationDecapode\", \"heat dissipates to the enviornment\", \"SummationDecapode\", \"v1.0\"),\n  Decapodes.parse_decapode(quote\n    Q₋::Form0{Point}\n    Q₀::Parameter{Point}\n    Q::Form0{Point}\n    λ::Constant{Point}\n\n    Q₋ == λ(Q-Q₀)\n  end)\n)\n\nsuperposition = ASKEMDecaExpr(\n  AMR.Header(\"LinearSuperpositon\", \"modelreps.io/SummationDecapode\", \"variables be addin\", \"SummationDecapode\", \"v1.0\"),\n  Decapodes.parse_decapode(quote\n    X::Form0{Point}\n    Y::Form0{Point}\n    T::Form0{Point}\n\n    T == X + Y\n  end)\n)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The CompositeModelExpr type can store recursive model descriptions in terms of compositions of composite models.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"h = AMR.Header(\"hierarchical_composite\", \"modelreps.io/Composite\", \"A hierarchical composite model of frictional heating\", \"CompositeModelExpr\", \"v0.1\")\nm = CompositeModelExpr(h,u, [OpenModel(d1, [:X, :V]),\n      CompositeModelExpr(AMR.Header(\"heating_dynamics\", \"modelreps.io/Composite\", \"A formula for heating - cooling\", \"CompositeModelExpr\", \"v0.1\"),\n        uwdʰ, [OpenModel(drag, [:V, :Q₊]), OpenModel(cooling, [:Q₋, :Q]), OpenModel(superposition, [:X, :Y, :T])])\n])","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The oapply function will recursively descend the tree to assemble a flat model with hierarchical namespacing.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"dh = apex(oapply(m))","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"This model can also be drawn as a decapode.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"to_graphviz(dh)","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"The new model description needs to be written by hand for the new model header. Some annotation in the description can't be avoided, yet.","category":"page"},{"location":"generated/literate_example/","page":"Composing Models","title":"Composing Models","text":"composite = OpenDecapode(m)\nhf = composite.model.header\nASKEMDecapode(Header(\"flattened_composite\", hf.schema, \"A flattened version of the composite_physics model.\", hf.schema_name, hf.model_version), composite.model.model)","category":"page"},{"location":"api/#Library-Reference","page":"Library Reference","title":"Library Reference","text":"","category":"section"},{"location":"api/","page":"Library Reference","title":"Library Reference","text":"Modules = [SyntacticModels, SyntacticModels.SyntacticModelsBase]","category":"page"},{"location":"api/#Base.Dict-Tuple{SyntacticModels.SyntacticModelsBase.AbstractTerm}","page":"Library Reference","title":"Base.Dict","text":"Dict(x::AbstractTerm)\n\nto register your type with JSON3, you need to overload JSON3.write to use this Dict approach. we only overload the Dict function for our type Formula, so this is not piracy.\n\n\n\n\n\n","category":"method"},{"location":"api/#StructTypes.StructType-Tuple{Type{SyntacticModels.SyntacticModelsBase.AbstractTerm}}","page":"Library Reference","title":"StructTypes.StructType","text":"StructTypes.StructType\n\nThis is how you tell StructTypes to use an interface for you.\n\n\n\n\n\n","category":"method"},{"location":"api/#SyntacticModels.SyntacticModelsBase.AbstractTerm","page":"Library Reference","title":"SyntacticModels.SyntacticModelsBase.AbstractTerm","text":"AbstractTerm\n\nThe super type for all SyntacticModels types. This abstract type exists so that we can write generic methods that work on any term in any of the domain specific syntaxes. For example, serializing to a Dictionary uses some reflection snippet that works for arbitrary types, but we only want to apply it to things that should be serialized like a Term.\n\n\n\n\n\n","category":"type"},{"location":"api/#JSON3.write-Tuple{SyntacticModels.SyntacticModelsBase.AbstractTerm}","page":"Library Reference","title":"JSON3.write","text":"JSON3.write\n\nNow JSON3.write(f) puts the type information in our reserved field.\n\n\n\n\n\n","category":"method"},{"location":"api/#StructTypes.subtypekey-Tuple{Type{SyntacticModels.SyntacticModelsBase.AbstractTerm}}","page":"Library Reference","title":"StructTypes.subtypekey","text":"StructTypes.subtypekey\n\nThis is how you tell StructTypes where to look for the name of a type when reading the type back in from JSON.\n\n\n\n\n\n","category":"method"},{"location":"api/#SyntacticModels.SyntacticModelsBase._dict-Tuple{T} where T<:SyntacticModels.SyntacticModelsBase.AbstractTerm","page":"Library Reference","title":"SyntacticModels.SyntacticModelsBase._dict","text":"_dict(x::T) where T <: AbstractTerm\n\nWe are going to convert our structs to Dict before we call JSON3.write and  add the type information in a generic construction. This uses some reflection  to ask the julia type for its fieldnames and then use those as the keys in the Dict. We use splatting, so don't make a struct with more than 32 fields if you want to go fast. We use this _dict function to avoid an \"I'm the captain now\" level of type piracy.\n\n\n\n\n\n","category":"method"},{"location":"api/#SyntacticModels.SyntacticModelsBase.typename_last-Tuple{Type}","page":"Library Reference","title":"SyntacticModels.SyntacticModelsBase.typename_last","text":"typename_last(T::Type)\n\nTruncate a type name keeping only the last components of the FQTN.\n\n\n\n\n\n","category":"method"},{"location":"api/#UWDs","page":"Library Reference","title":"UWDs","text":"","category":"section"},{"location":"api/","page":"Library Reference","title":"Library Reference","text":"Modules = [SyntacticModels.ASKEMUWDs]","category":"page"},{"location":"api/#SyntacticModels.ASKEMUWDs.UWDTerm","page":"Library Reference","title":"SyntacticModels.ASKEMUWDs.UWDTerm","text":"UWDTerm\n\nTerm specifying UWD.\n\nSubtypes\n\nUWDModel: A header and UWD Expr\nUWDExpr: A Context of variables and a list of statements defining a UWD\nStatement: R(x,y,z) a relation that acts on its arguments (which are Vars)\n\nExample\n\nTo specify the following relation macro:\n\n@relation (x:X, z:Z) where y:Y begin\n  R(x,y)\n  S(y,z)\n  T(z,y,u)\nend\n\nUse the following SyntacticModels UWDTerm:\n\nv1 = Typed(:x, :X)\nv2 = Typed(:y, :Y)\nv3 = Typed(:z, :Z)\nv4 = Untyped(:u)\nc = [v1, v3]\ns = [Statement(:R, [v1,v2]),\n  Statement(:S, [v2,v3]),\n  Statement(:T, [v3,v2, v4])]\nu = UWDExpr(c, s)\n\n\n\n\n\n","category":"type"},{"location":"api/#SyntacticModels.ASKEMUWDs.Var","page":"Library Reference","title":"SyntacticModels.ASKEMUWDs.Var","text":"Var\n\nVariables of a UWD. Types are the domain types, ScalarField, VectorField, Dual1Form, Primal2Form NOT Float64,Complex128\n\nSubtypes include:\n\nUntyped(var::Symbol)\nTyped(var::Symbol, type::Symbol)\n\nwhich are used for representing typed or untyped variables.\n\n\n\n\n\n","category":"type"},{"location":"api/#SyntacticModels.ASKEMUWDs.construct-Tuple{Type{Catlab.Programs.RelationalPrograms.RelationDiagram}, SyntacticModels.ASKEMUWDs.UWDExpr}","page":"Library Reference","title":"SyntacticModels.ASKEMUWDs.construct","text":"construct(::Type{RelationDiagram}, ex::UWDExpr)\n\nBuilds a RelationDiagram from a UWDExpr like the @relation macro does for Julia Exprs.\n\n\n\n\n\n","category":"method"},{"location":"api/#Decapodes","page":"Library Reference","title":"Decapodes","text":"","category":"section"},{"location":"api/","page":"Library Reference","title":"Library Reference","text":"Modules = [SyntacticModels.ASKEMDecapodes]","category":"page"},{"location":"api/#SyntacticModels.ASKEMDecapodes.ASKEMDeca","page":"Library Reference","title":"SyntacticModels.ASKEMDecapodes.ASKEMDeca","text":"ASKEMDeca\n\nStores a Decapode with the model metadata for ASKEM AMR conformance.\n\n\n\n\n\n","category":"type"},{"location":"api/#SyntacticModels.ASKEMDecapodes.ASKEMDecaExpr","page":"Library Reference","title":"SyntacticModels.ASKEMDecapodes.ASKEMDecaExpr","text":"ASKEMDecaExpr\n\nStores the syntactic expression of a Decapode Expression with the model metadata for ASKEM AMR conformance.\n\n\n\n\n\n","category":"type"},{"location":"api/#SyntacticModels.ASKEMDecapodes.ASKEMDecapode","page":"Library Reference","title":"SyntacticModels.ASKEMDecapodes.ASKEMDecapode","text":"ASKEMDecapode\n\nStores the combinatorial representation of a Decapode with the model metadata for ASKEM AMR conformance.\n\n\n\n\n\n","category":"type"},{"location":"api/#Composites","page":"Library Reference","title":"Composites","text":"","category":"section"},{"location":"api/","page":"Library Reference","title":"Library Reference","text":"Modules = [SyntacticModels.Composites]","category":"page"},{"location":"api/#SyntacticModels.Composites.CompositeModel","page":"Library Reference","title":"SyntacticModels.Composites.CompositeModel","text":"CompositeModel\n\n@data CompositeModel <: AbstractTerm begin\n  OpenModel(model::ASKEMDecapodes.ASKEMDecaExpr, interface::Vector{Symbol})\n  OpenDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})\n  CompositeModelExpr(header::Header, composition_pattern::UWDExpr, components::Vector{CompositeModel})\nend\n\n\n\n\n\n","category":"type"},{"location":"api/#Catlab.WiringDiagrams.WiringDiagramAlgebras.oapply-Tuple{SyntacticModels.Composites.CompositeModel}","page":"Library Reference","title":"Catlab.WiringDiagrams.WiringDiagramAlgebras.oapply","text":"Catlab.oapply(m::CompositeModel)\n\nCompositeModels can be flattened into a single level of model with the oapply function.\n\nwarning: Warning\nBecause the oapply algorithm operates on the compute graph representation of the equations, it does not produce syntactic equations.  Calls to oapply produce instances of OpenDecapode and not DecaExpr.  Software that expects to consume decapodes should plan to interact with both forms.\n\n\n\n\n\n","category":"method"},{"location":"api/#SyntacticModels.Composites.interface-Tuple{SyntacticModels.Composites.CompositeModel}","page":"Library Reference","title":"SyntacticModels.Composites.interface","text":"interface(m::CompositeModel)\n\nExtract the interface of a composite model. If the model is open, then it is the feet of the cospan. If it is a Composite, then it is the context of the uwd.\n\n\n\n\n\n","category":"method"},{"location":"#SyntacticModels.jl","page":"SyntacticModels.jl","title":"SyntacticModels.jl","text":"","category":"section"},{"location":"","page":"SyntacticModels.jl","title":"SyntacticModels.jl","text":"CurrentModule = SyntacticModels","category":"page"},{"location":"","page":"SyntacticModels.jl","title":"SyntacticModels.jl","text":"SyntacticModels.jl is a Julia library for representing models as syntactic expressions. ","category":"page"},{"location":"","page":"SyntacticModels.jl","title":"SyntacticModels.jl","text":"The driving example for this library is the need to interoperate models between programming languages in the DARPA ASKEM Program. The AlgebraicJulia ecosystem includes some great tools for specifying modeling languages, but they are deeply connected to the Julia language. This package aims to provide simple tools for specifying domain specific programming languages that can be used to exchange the specification of scientific models between host languages.","category":"page"},{"location":"","page":"SyntacticModels.jl","title":"SyntacticModels.jl","text":"We heavily use the MLStyle.jl package for defining Algebraic Data Types so you should familiarize yourself with those concepts before reading on in this documentation.","category":"page"}]
}