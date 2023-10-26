using Test

using SyntacticModels

@testset "DTypes" begin
  include("dtypes/DTypes.jl")
end

@testset "Core" begin
  include("core.jl")
end
