module SyntacticModels

#-----------------------------------------------------------------------------# "SyntacticModelsBase"
using InteractiveUtils: subtypes
using StructTypes

"""    AbstractTerm

The super type for all SyntacticModsels types. This abstract type exists so that we can write generic methods that work on any term in any of the domain specific syntaxes.
For example, serializing to a Dictionary uses some reflection snippet that works for arbitrary types, but we only want to apply it to things that should be serialized like a Term.
"""
abstract type AbstractTerm end

function StructTypes.StructType(::Type{T}) where {T <: AbstractTerm}
    isconcretetype(T) ? StructTypes.CustomStruct() : StructTypes.AbstractType()
end


# lowering/unlowering
function StructTypes.lower(x::T) where {T <: AbstractTerm}
    (_type = T.name.name, NamedTuple(k => getfield(x, k) for k in fieldnames(T))...)
end

function StructTypes.lowertype(::Type{T}) where {T <: AbstractTerm}
    NamedTuple{(:_type, fieldnames(T)...), Tuple{Symbol, fieldtypes(T)...}}
end

# Doesn't work with 1-field structs
# (::Type{T})(x::StructTypes.lowertype(Type{T})) where {T <: AbstractTerm} = T(x[fieldnames(T)]...)


function concrete_subtypes(T)
    out = Type[]
    for S in subtypes(T)
        isconcretetype(S) ? push!(out, S) : append!(out, concrete_subtypes(S))
    end
    return out
end

StructTypes.subtypes(::Type{T}) where {T <: AbstractTerm} = Dict(T.name.name => T for T in concrete_subtypes(T))

StructTypes.subtypekey(T::Type{<: AbstractTerm}) = :_type


#-----------------------------------------------------------------------------# includes
include("amr.jl")
include("decapodes.jl")
include("uwd.jl")
include("composite_models.jl")

#-----------------------------------------------------------------------------# Constructors
for T in concrete_subtypes(AbstractTerm)
    @eval function $(parentmodule(T)).$(T.name.name)(x::NamedTuple)
        fields = filter(x -> x != :_type, fieldnames($T))
        args = collect(x[fields])
        @show args
        $(parentmodule(T)).$(T.name.name)(args...)
    end
end

end
