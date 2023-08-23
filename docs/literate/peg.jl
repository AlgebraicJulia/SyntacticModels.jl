using PEG
PEG.setdebug!(false)

using MLStyle
using SyntacticModels
using SyntacticModels.ASKEMUWDs
using Test
using Base.Iterators

cflat = collect∘flatten

build_context(parenmatch) = map(Untyped∘Symbol, filter(s->s!=",", parenmatch[2]))
@rule item = r"[^,]*"w |> string
@rule commalist = (item & r"[,]"p)[*] & item |> (cflat ∘ cflat)
@rule parens = r"\(" & commalist & r"\)" |> build_context

commalist("a,b")

@testset "Rules" begin
@testset "Words" begin
  @test item("a,b") == ("a", ",b")
  @test item("a, b") == ("a", ", b")
  @test item("a ,b") == ("a", ",b")
  @test item("a , b") == ("a", ", b")
end

@testset "Commalist" begin
  v = (["a", ",", 'b'])
  v₂ = (["a", ",", 'b'])
  @test commalist("a,b")[1] == v
  @test commalist("a ,b")[1] == v₂
  @test commalist("a, b")[1] == ["a", ",", 'b']
  @test commalist("a , b")[1] == ["a", ",", 'b']
  @test commalist("a, b,c")[1] == ["a", ",", "b", ",", 'c']
  @test commalist("a, b,c, d")[1] == ["a", ",", "b", ",", "c", ",", 'd']
end

end


@testset "Parens" begin
  v = [Untyped(:a), Untyped(:b)]
  # vₜ = [Untyped(Symbol("a:A")), Untyped(Symbol("b:B"))]
  @test parens("(a,b)")[1] == v
  @test parens("(a ,b)")[1] == v
  @test parens("(a, b)")[1] == v
  # @test parens("(a:A, b:B)")[1] == vₜ
end



# parens("R(a,b)")