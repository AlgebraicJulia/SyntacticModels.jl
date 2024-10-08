module ASKEMUWDs

# include("amr.jl")
export Var, Typed, Untyped, Statement, UWDExpr, UWDModel, UWDTerm, context

using ..SyntacticModelsBase
using ..AMR

using MLStyle
using StructTypes
using Catlab
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
import Base: show


@data Var <: AbstractTerm begin
  Untyped(var::Symbol)
  Typed(var::Symbol, type::Symbol)
end

@doc """    Var

Variables of a UWD. Types are the domain types, ScalarField, VectorField, Dual1Form, Primal2Form NOT Float64,Complex128

Subtypes include:

1. Untyped(var::Symbol)
1. Typed(var::Symbol, type::Symbol)

which are used for representing typed or untyped variables.
"""
Var

StructTypes.StructType(::Type{Var}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{Var}) = :_type
StructTypes.subtypes(::Type{Var}) = (Untyped=Untyped, Typed=Typed)

@data UWDTerm <: AbstractTerm begin
  Statement(relation::Symbol, variables::Vector{Var})
  UWDExpr(context::Vector{Var}, statements::Vector{Statement})
  UWDModel(header::AMR.Header, uwd::UWDExpr)
end

@doc """    UWDTerm

Term specifying UWD.

Subtypes
========

1. UWDModel: A header and UWD Expr
1. UWDExpr: A Context of variables and a list of statements defining a UWD
1. Statement: R(x,y,z) a relation that acts on its arguments (which are Vars)

Example
=======

To specify the following relation macro:
```julia
@relation (x, z) where (x::X, y::Y, z::Z, u::U) begin
  R(x,y)
  S(y,z)
  T(z,y,u)
end
```

Use the following SyntacticModels UWDTerm:

```julia
v1 = Typed(:x, :X)
v2 = Typed(:y, :Y)
v3 = Typed(:z, :Z)
v4 = Untyped(:u)
c = [v1, v3]
s = [Statement(:R, [v1,v2]),
  Statement(:S, [v2,v3]),
  Statement(:T, [v3,v2, v4])]
u = UWDExpr(c, s)
```
"""
UWDTerm

StructTypes.StructType(::Type{UWDTerm}) = StructTypes.AbstractType()
StructTypes.subtypekey(::Type{UWDTerm}) = :_type
StructTypes.subtypes(::Type{UWDTerm}) = (Statement=Statement, UWDExpr=UWDExpr, UWDModel=UWDModel)

varname(v::Var) = @match v begin
  Untyped(v) => v
  Typed(v, t) => v
end

vartype(v::Var) = @match v begin
  Typed(v, t) => t
  Untyped(v) => :untyped
end

context(t::UWDTerm) = @match t begin
  Statement(R, xs) => xs
  UWDExpr(context, statements) => context
  UWDModel(h, uwd) => context(uwd)
end

"""    show(io::IO, s::UWDTerm)

generates a human readable string of the `UWDTerm` (or any sub-term).
"""
function show(io::IO, s::UWDTerm)
  let ! = show
    @match s begin
      Statement(r, v) => begin print(io, "$r("); show(io, v, wrap=false); print(io, ")") end
      UWDExpr(c, body) => begin 
        map(enumerate(body)) do (i,s)
          if i == 1
            print(io, "{ ")
            show(io, s)
            print(io, "\n")
          elseif i == length(body)
            print(io, "  ")
            show(io, s)
            print(io, " }")
          else
            print(io, "  ")
            show(io, s)
            print(io, "\n")
          end
        end
        print(io, " where ")
        show(io, c)
      end
      UWDModel(h, uwd) => begin println(io, amr_to_string(h)); println(io, "UWD:"); !(io, uwd); end
    end
  end
end

function show(io::IO, c::Vector{Var}; wrap=true)
  if wrap
    print(io, "{")
  end
  map(enumerate(c)) do (i,s)
    @match s begin
      Untyped(v) => print(io, v)
      Typed(v, T) => print(io, "$v:$T")
    end
    if i != length(c)
      print(io, ", ")
    end
  end
  if wrap
    print(io, "}")
  end
end

"""    construct(::Type{RelationDiagram}, ex::UWDExpr)

Builds a RelationDiagram from a UWDExpr like the `@relation` macro does for Julia Exprs.
"""
function construct(::Type{RelationDiagram}, ex::UWDExpr)
  # If you want to understand this code, look at the schema for Relation Diagrams
  # to_graphviz(RelationalPrograms.SchRelationDiagram)
  uwd = RelationDiagram(map(varname, ex.context))
  junctions = Dict()
  # first we add in all the outer ports and make junctions for them.
  for (i,j) in enumerate(ex.context)
    k = add_part!(uwd, :Junction, variable=varname(j), junction_type=vartype(j))
    junctions[varname(j)] = k
    set_subpart!(uwd, i, :outer_junction, k)
  end

  # then for each statement we add a box, and its ports
  for s in ex.statements
    b = add_part!(uwd, :Box, name=s.relation)
    for a in s.variables
      # if a junction is missing, we have to add it. This is for nonexported variables
      if !(varname(a) ∈ keys(junctions))
        k = add_part!(uwd, :Junction, variable=varname(a), junction_type=vartype(a))
        junctions[varname(a)] = k
      end
      # every port connects to the junction with the same variable name
      add_part!(uwd, :Port, box=b, port_type=vartype(a), junction=junctions[varname(a)])
    end
  end
  return uwd
end
end