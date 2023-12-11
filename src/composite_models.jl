module Composites

export CompositeModelExpr, OpenModel, OpenDecapode, CompositeModel, interface, open_decapode, oapply, Open

using Catlab

using ..AMR
using ..ASKEMDecapodes
using ..ASKEMUWDs

using ACSets
using ACSets.InterTypes

using ..AMR.amr
using ..ASKEMDecapodes.decapodes
using ..ASKEMUWDs.uwd

@intertypes "../src/composite_models.it" module composites
  import ..amr
  import ..decapodes
  import ..uwd
end

using .composites

"""    interface(m::CompositeModel)

Extract the interface of a composite model. If the model is open, then it is the feet of the cospan. If it is a Composite, then it is the context of the uwd.
"""
interface(m::CompositeModel) = @match m begin
  OpenModel(M, I) => I
  CompositeModelExpr(h, uwd′, components) => map(ASKEMUWDs.varname, context(uwd′))
end

using Decapodes

function it_to_orig(test::decapodes.SymSummationDecapode)
  d = Decapodes.SummationDecapode{Any, Any, Symbol}()
  copy_parts!(d,test, NamedTuple(Dict(k=>parts(test,k) for k in types(decapodes.SchSummationDecapode))))
  return d
end

# OpenSummationDecapodeOb, OpenSummationDecapode = OpenACSetTypes(Decapodes.SummationDecapode, :Var)

#=
function Open(d::decapodes.SummationDecapode, names::Vector{Symbol})
    legs = map(names) do name
    FinFunction(incident(d, name, :name), nparts(d, :Var))
  end
  OpenSummationDecapode(d, legs...)
end
=#

#=
apex(decapode::OpenSummationDecapode) = apex(decapode.cospan)
legs(decapode::OpenSummationDecapode) = legs(decapode.cospan)
feet(decapode::OpenSummationDecapode) = decapode.feet
=#

# Extract an open decapode from the decapode expression and the interface
open_decapode(d, interface) = Decapodes.Open(it_to_orig(ASKEMDecapodes.SummationDecapode(d.model)), interface)
open_decapode(d::ASKEMDecaExpr, interface) = Decapodes.Open(it_to_orig(ASKEMDecapodes.SummationDecapode(d.model)), interface)
open_decapode(d::ASKEMDecapode, interface) = Decapodes.Open(it_to_orig(d.model), interface)

"""    Catlab.oapply(m::CompositeModel)

CompositeModels can be flattened into a single level of model with the oapply function.

!!! warning 
    Because the oapply algorithm operates on the compute graph representation of the equations, it does not produce syntactic equations. 
    Calls to oapply produce instances of OpenDecapode and not DecaExpr. 
    Software that expects to consume decapodes should plan to interact with both forms.
"""
function Catlab.oapply(m::CompositeModel)
  let ! = oapply
    @match m begin
      # For a primitive model we just attach the interface
      OpenModel(M, I) => open_decapode(M,I)
      OpenDecapode(M, I) => open_decapode(M,I)
      # For a composite model, we have to recurse
      CompositeModelExpr(h, pattern, components) => begin
        uwd = ASKEMUWDs.construct(RelationDiagram, pattern)
        Ms = map(m.components) do mᵢ; 
          !(mᵢ) # oapply all the component models recursively
        end
        # OpenDecapode(ASKEMDecapode(h, apex(!(uwd, Ms))), interface(m)) # Then we call the oapply from Decapodes.
        Decapodes.Open(apex(!(uwd, Ms)), uwd[[:outer_junction, :variable]]) # Then we call the oapply from Decapodes.
      end
    end
  end
end

#=
function OpenDecapode(m::CompositeModel)
  composite = oapply(m)
  feet = map(l->only(dom(l)[:name]), legs(composite))
  OpenDecapode(ASKEMDecapode(m.header,apex(composite)), feet)
end
=#

end