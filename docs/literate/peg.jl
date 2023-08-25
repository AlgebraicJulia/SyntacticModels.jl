using PEG
PEG.setdebug!(false)

using MLStyle
using SyntacticModels
using SyntacticModels.ASKEMUWDs
using Test
using Base.Iterators

cflat = collect∘flatten

build_context(parenmatch) = map(Untyped∘Symbol, filter(s->s!=",", parenmatch[2]))

# Create some rules 

@rule item = r"[^,]*"w |> string
@rule commalist = (item & r"[,]"p)[*] & item |> (cflat ∘ cflat)
@rule parens = r"\(" & commalist & r"\)" |> build_context


# @rule schemadef = "schema: " & name & body
@rule body = r"{"p & line[*]  & r"}"p

@rule line = ws[*] & statement & ws[*] & EOL

@rule elname = r"[^:{}→\n;=,]*"
@rule obname = r"[^:{}→\n;=,]*"

@rule EOL = "\n" , ";"
@rule nonclosing = r"[^}]"p
@rule type = nonclosing[*]
@rule name = r"[^{]*"
@rule int = r"\d+"
@rule colon = r":"p
@rule range = int & colon & int
@rule judgement = elname & colon & obname & r"," |> Typed
@rule finjudgement = elname & colon & obname |> Typed
@rule ws = r"\s*"
@rule eq = r"="p

@rule instance = "instance: " & name & instbody
@rule instbody = r"{"p & (leaftable , table)[*] & r"}"p
@rule leaftable = obname & r"="p & range
@rule table = ws & name & r"{"p & row[*] & r"}"p
@rule row = ws & pair[*] & finpair & EOL
@rule pair = key & r"="h & value & r","p
@rule finpair = key & r"="h & value
@rule key = r"[^,=\n]*"
@rule value = r"[^,=\n]*"

@rule context = r"{"p & judgement[*] & finjudgement & r"}"p |> buildcontext

@rule lparen = r"{"p
@rule rparen = r"}"p
@rule comma = r","p
@rule finarg = elname
@rule arg = elname & comma
@rule statement = elname & lparen & arg[*] & finarg & rparen |> Statement

ASKEMUWDs.Typed(j::Vector{Any}) = begin
  Typed(Symbol(j[1]), Symbol(j[3]))
end

buildcontext(v::Vector{Any}) = begin
  push!(v[2], v[3])
  return v[2]
end

ASKEMUWDs.Statement(v::Vector{Any}) = begin
  args = (Untyped∘Symbol∘first).(v[3])
  push!(args, Untyped(Symbol(v[4])))
  Statement(Symbol(v[1]), args)
end

@test judgement("a:A,")[1] == Typed(:a, :A)
@test judgement("ab:AB,")[1] == Typed(:ab, :AB)

@test finjudgement("a:A")[1] == Typed(:a, :A)
@test finjudgement("ab:AB")[1] == Typed(:ab, :AB)

@test context("{a:A,b:B}")[1] == [Typed(:a, :A), Typed(:b, :B)]

@show statement("R{a,b}")

@show statement("S{u,b}")
# body("""{R(a,b)
# S(u,b)}""")

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
  @test parens("(a,b)")[1] == v
  @test parens("(a ,b)")[1] == v
  @test parens("(a, b)")[1] == v
end


# @test parens("(a:A, b:B)")[1] == vₜ
# vₜ = [Untyped(Symbol("a:A")), Untyped(Symbol("b:B"))]

# parens("R(a,b)")