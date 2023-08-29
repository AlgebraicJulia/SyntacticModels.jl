using Test

using SyntacticModels

@testset "Core" begin
  include("core.jl")
end

@testset "Parsers" begin
  include("parsers.jl")
end
