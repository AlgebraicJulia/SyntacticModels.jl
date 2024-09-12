using Test

using JSON3
using SyntacticModels
using SyntacticModels.ASKEMDecapodes

jsondir = joinpath(@__DIR__, "json")

write_json_model(m, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  JSON3.pretty(fp, Dict(m))
end

readback(m, prefix=joinpath(@__DIR__, "json")) = open(joinpath(jsondir, "$(m.header.name).json"), "r") do fp
  JSON3.read(fp, typeof(m))
end

write_json_model(m::ASKEMDecapodes.ASKEMDecapode, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  d = Dict("header"=>m.header, "model"=>generate_json_acset(m.model), "_type"=> "ASKEMDecapode")
  JSON3.pretty(fp, d)
end

sm_write_json_acset(X, fname, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(fname).json"), "w") do fp
  JSON3.pretty(fp, generate_json_acset(X))
end

try
  mkdir(joinpath(@__DIR__, "json"))
catch
  @info "JSON DIR already exists, you might want to rm -r it to clean up"
end

@testset "AMR Examples" begin
  include("amr_examples.jl")
end

@testset "Decapodes Examples" begin
  include("decapodes_examples.jl")
end

@testset "UWD Examples" begin
  include("uwd_examples.jl")
end

@testset "Parsers" begin
  include("parsers.jl")
end

@testset "Composite Model Examples" begin
  include("composite_models_examples.jl")
end

@testset "Serialization Minimum Working Example" begin
  include("serialization_mwe.jl")
end