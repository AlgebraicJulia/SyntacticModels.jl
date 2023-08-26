# # Parsing UWD expressions with PEG

# Sometimes we want to be able to represent our models in a totally custom grammar that isn't tied
# to any particular programming language syntax. In this case we want to describe undirected wiring diagrams
# in a domain specific language based on the traditional syntax for mathematical relations, plus some
# syntax inspired by C.

using PEG
PEG.setdebug!(false)
using MLStyle
using SyntacticModels
using SyntacticModels.ASKEMUWDs
using Test
using Base.Iterators

Base.:(==)(s::Statement, t::Statement) = s.relation == t.relation && s.variables == t.variables
Base.:(==)(s::Untyped, t::Untyped) = s.var == t.var

cflat = collect∘flatten

build_context(parenmatch) = map(Untyped∘Symbol, filter(s->s!=",", parenmatch[2]))

# ## Create some rules 
# These basic rules are for *lexing*, they define character classes that will help us
# break up text into words or other syntactic constructs. They should be reused between
# grammars as the lowest level of syntactic structure.

@rule ws = r"\s*"
@rule eq = r"="p
@rule lparen = r"\("
@rule rparen = r"\)"
@rule comma = r","p
@rule EOL = "\n" , ";"
@rule colon = r":"p
@rule elname = r"[^:{}→\n;=,\(\)]*"
@rule obname = r"[^:{}→\n;=,\(\)]*"

# Now we get to the syntax structures specific to our DSL.
# A judgement is an of the form x:X. We need to handle the items in the middle of list and the last item separately.
# It would be nice to have a better way to do this, but basically anything that can occur in a list has two rules associated with it.
# We use the prefix `fin` for final.

@rule judgement = elname & colon & obname & r"," |> Typed
@rule finjudgement = elname & colon & obname |> Typed

# A context is a list of judgements between brackets. When a rule ends with `|> f`
# it means to call `f` on the result of the parser inside the recursion.
# We are using these functions to get more structured output as we pop the function call stack.
# we don't want to end up with an `Array{Any}` that is deeply nested as the return value of our parse.

@rule context = r"{"p & judgement[*] & finjudgement & r"}"p |> buildcontext

# Our statements  are of the form `R(a,b,c)`. A name(list of names).
@rule statement = elname & lparen & arg[*] & finarg & rparen |> Statement
@rule arg = elname & comma
@rule finarg = elname

# A line is statement, with some whitespace ended by a EOL character.
# The body of our relational program is a list of lines between braces.

@rule line = ws & statement & ws & EOL |> v->v[2]
@rule body = r"{\s*"p & line[*]  & r"\n?}"p |> v->v[2]

# The UWD is a body and then a context for it separated by the word "where".

@rule uwd = body & ws & "where" & ws & context |> v -> UWDExpr(v[end], v[1])

# Some of our rules construct higher level structures for the results. Those methods are defined here:

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

# ## Testing our parser

# Let's make sure we can parse the relational program syntax we designed!

uwd("""{R(a,b); S(b,c);} where {a:A,b:B,c:C}""")[1]

# Now we write some unit tests. This is how I wrote this code, by writing the tests from the bottom up.

@testset "Parens" begin
  @test lparen("(")[1] == "("
  @test rparen(")")[1] == ")"
  @test elname("R(a)")[1] == "R"
end

@testset "Judgements" begin
  @test judgement("a:A,")[1] == Typed(:a, :A)
  @test judgement("ab:AB,")[1] == Typed(:ab, :AB)

  @test finjudgement("a:A")[1] == Typed(:a, :A)
  @test finjudgement("ab:AB")[1] == Typed(:ab, :AB)
end

@testset "Contexts" begin
  @test context("{a:A,b:B}")[1] == [Typed(:a, :A), Typed(:b, :B)]
end


@testset "Statements" begin
  @test [Untyped(:u)] == [Untyped(:u)]
  @test statement("R(a,b)")[1] == Statement(:R, [Untyped(:a),Untyped(:b)])
  @test statement("S(u,b)")[1] == Statement(:S, [Untyped(:u),Untyped(:b)])
  @test statement("S(u,b,x)")[1].relation == Statement(:S, [Untyped(:u), Untyped(:b), Untyped(:x)]).relation
  @test statement("S(u,b,x)")[1].variables == Statement(:S, [Untyped(:u), Untyped(:b), Untyped(:x)]).variables
  @test statement("S(u)")[1].relation == Statement(:S, [Untyped(:u)]).relation
  @test statement("S(u)")[1].variables == Statement(:S, Var[Untyped(:u)]).variables
end

@testset "Body" begin
  @test body("""{
  R(a,b);}""")[1][1] isa Statement

  @test body("""{
  R(a,b);
  }""")[1][1] isa Statement

  @test body("""{
    R(a,b);
  }""")[1][1] isa Statement

  @test length(body("""{
  R(a,b);
    S(u,b);
  }""")[1]) == 2
end

# Our final test shows that we can parse what we expect to be able to parse:
@testset "UWD" begin
  @test uwd("""{R(a,b); S(b,c);} where {a:A,b:B,c:C}""")[1].context == [Typed(:a, :A), Typed(:b,:B), Typed(:c,:C)]
  @test uwd("""{R(a,b); S(b,c);}
   where {a:A,b:B,c:C}""")[1].statements == [Statement(:R, [Untyped(:a), Untyped(:b)]),
    Statement(:S, [Untyped(:b), Untyped(:c)])]
end
