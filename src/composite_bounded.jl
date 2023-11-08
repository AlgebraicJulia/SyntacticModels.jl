module BoundedComposites

export CompositeModelExpr, OpenModel, OpenDecapode, OpenBoundedDecapode, CompositeModel, interface, open_decapode

using MLStyle
using Catlab
using Decapodes
using StructTypes

using ..SyntacticModelsBase
using ..AMR
using ..ASKEMDecapodes
using ..ASKEMUWDs
using ..ASKEMCollages

@data CompositeModel <: AbstractTerm begin
  # OpenModel(model::ASKEMDecapodes.ASKEMDecaExpr, interface::Vector{Symbol})
  # OpenDecapode(model::ASKEMDecapodes.ASKEMDecapode, interface::Vector{Symbol})
  # OpenBCDecapode(model::ASKEMCollages.ASKEMCollageBC, interface::Vector{Symbol})
  # OpenICDecapode(model::ASKEMCollages.ASKEMCollageIC, interface::Vector{Symbol})
  OpenBoundedDecapode(model::ASKEMCollages.ASKEMCollage, interface::Vector{Symbol})
  # OpenBoundedDecapode(header::Header, model::Decapodes.Collage, interface::Vector{Symbol})
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
@doc """    OpenBoundedDecapode

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
OpenBoundedDecapode
@doc """    CompositeModelExpr

Stores the syntactic expression of a Decapode Expression with the
model metadata for ASKEM AMR conformance.
"""
CompositeModelExpr

StructTypes.StructType(::Type{CompositeModel}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{CompositeModel}) = :_type
StructTypes.subtypes(::Type{CompositeModel}) = (OpenBoundedDecapode=OpenBoundedDecapode, CompositeModelExpr) # OpenModel=OpenModel, OpenDecapode=OpenDecapode, 


"""    interface(m::CompositeModel)

Extract the interface of a composite model. If the model is open, then it is the feet of the cospan. If it is a Composite, then it is the context of the uwd.
"""
interface(m::CompositeModel) = @match m begin
  # OpenModel(M, I) => I
  # OpenBCDecapode(M, I) => I
  # OpenICDecapode(M, I) => I
  OpenBoundedDecapode(M, I) => I
  CompositeModelExpr(h, uwd, components) => map(ASKEMUWDs.varname, context(uwd))
end

# Extract an open decapode from the decapode expression and the interface
# open_decapode(d, interface) = Open(SummationDecapode(d.model), interface)
# open_decapode(d::ASKEMDecaExpr, interface) = Open(SummationDecapode(d.model), interface)
# open_decapode(d::ASKEMDecapode, interface) = Open(d.model, interface)
open_decapode(d::ASKEMCollage, interface) = begin
  Open(d.model.bc.morphism.codom, interface), [d.model.bc], [d.model.ic]
end

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
      # OpenModel(M, I) => open_decapode(M,I)
      # OpenDecapode(M, I) => open_decapode(M,I)
      # OpenBCDecapode(M, I) => open_decapode(M,I)
      # OpenICDecapode(M, I) => open_decapode(M,I)
      OpenBoundedDecapode(M, I) => begin
        composite, bc, ic = open_decapode(M,I)
      end
      # For a composite model, we have to recurse
      CompositeModelExpr(h, pattern, components) => begin
        uwd = ASKEMUWDs.construct(RelationDiagram, pattern)
        M_w_bounds = map(m.components) do mᵢ; 
          !(mᵢ) # oapply all the component models recursively
        end
        Ms, BCs, ICs = broadcast(vcat,M_w_bounds...)
        # OpenDecapode(ASKEMDecapode(h, apex(!(uwd, Ms))), interface(m)) # Then we call the oapply from Decapodes.
        diag = !(uwd, Ms)
        new_BCs = []
        for old_bc in BCs
          # for old_bc in old_comp_bcs
            new_tgts = map(x -> only(incident(apex(diag),old_bc.morphism.codom[:name][old_bc.morphism.components[:Var].func][x],:name)),
                                    dom(old_bc.morphism.components.Var))
            push!(new_BCs,ACSetTransformation(old_bc.morphism.dom,apex(diag), Var = new_tgts)) #BCMorphism
          # end
        end
        new_ICs = []
        for old_ic in ICs
          # for old_ic in old_comp_ics
            new_tgts = map(x -> only(incident(apex(diag),old_ic.morphism.codom[:name][old_ic.morphism.components[:Var].func][x],:name)),
                                    dom(old_ic.morphism.components.Var))
            push!(new_ICs,ACSetTransformation(old_ic.morphism.dom,apex(diag), Var = new_tgts)) #ICMorphism
          # end
        end
        Open(apex(diag), uwd[[:outer_junction, :variable]]), new_BCs, new_ICs # Then we call the oapply from Decapodes.
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

function OpenDecapode(m::CompositeModel)
  composite, bc, ic = oapply(m)
  feet = map(l->only(dom(l)[:name]), legs(composite))
  OpenDecapode(apex(composite),feet), bc ,ic
end


#=
abstract type AbstractCollage end

struct Collage <: AbstractCollage
  bc::BCMorphism
  ic::ICMorphism
end
=#

#=
function OpenBoundedDecapode(m::CompositeModel)
  composite, bc, ic = oapply(m)
  feet = map(l->only(dom(l)[:name]), legs(composite))
  OpenBoundedDecapode(m.header,Open(apex(composite),feet), Collage(bc, ic))
end

function OpenBoundedDecapode(m::ASKEMCollage, interface)
  OpenBoundedDecapode(m.header,Open(m.model.bc.morphism.codom,interface), m.model)
end
=# 

end