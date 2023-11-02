module Composites

export CompositeModelExpr, OpenModel, OpenDecapode, CompositeModel, interface, open_decapode

using MLStyle
using Catlab
using Decapodes
using StructTypes

using ..SyntacticModelsBase
using ..AMR
using ..ASKEMDecapodes
using ..ASKEMUWDs

@data CompositeModel <: AbstractTerm begin
  OpenModel(model::ASKEMDecapodes.ASKEMDecaExpr, interface::Vector{Symbol})
  OpenDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  # OpenBCDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  # OpenICDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  # OpenBoundedDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  CompositeModelExpr(header::Header, composition_pattern::UWDExpr, components::Vector{CompositeModel})
end

@doc """    CompositeModel

```julia
@data CompositeModel <: AbstractTerm begin
  OpenModel(model::ASKEMDecapodes.ASKEMDecaExpr, interface::Vector{Symbol})
  OpenDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  CompositeModelExpr(header::Header, composition_pattern::UWDExpr, components::Vector{CompositeModel})
end
```
"""
CompositeModel

StructTypes.StructType(::Type{CompositeModel}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{CompositeModel}) = :_type
StructTypes.subtypes(::Type{CompositeModel}) = (OpenModel=OpenModel, OpenDecapode=OpenDecapode, CompositeModelExpr)


"""    interface(m::CompositeModel)

Extract the interface of a composite model. If the model is open, then it is the feet of the cospan. If it is a Composite, then it is the context of the uwd.
"""
interface(m::CompositeModel) = @match m begin
  OpenModel(M, I) => I
  CompositeModelExpr(h, uwd, components) => map(ASKEMUWDs.varname, context(uwd))
end

# Extract an open decapode from the decapode expression and the interface
open_decapode(d, interface) = Open(SummationDecapode(d.model), interface)
open_decapode(d::ASKEMDecaExpr, interface) = Open(SummationDecapode(d.model), interface)
open_decapode(d::ASKEMDecapode, interface) = Open(d.model, interface)

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
        Open(apex(!(uwd, Ms)), uwd[[:outer_junction, :variable]]) # Then we call the oapply from Decapodes.
      end
    end
  end
end

function OpenDecapode(m::CompositeModel)
  composite = oapply(m)
  feet = map(l->only(dom(l)[:name]), legs(composite))
  OpenDecapode(ASKEMDecapode(m.header,apex(composite)), feet)
end

end