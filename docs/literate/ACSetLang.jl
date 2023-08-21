# # Custom Syntax for ACSet Languages
#
# We can define a mini language for the specification of ACSets that is based on a PEG grammar
# This syntax lets you define a language independent encoding of ACSets into plain text.
# The parser is generated from a specification of the grammatical rules.

using PEG
using Test

# Create some rules 

@rule grammar = schemadef 
@rule schemadef = "schema: " & name & body
@rule body = r"{"p & (obline , hom, attr)[*]  & r"}"p

@rule obline = ob & EOL
@rule ob = obname & r"::Ob"
@rule obname = r"[^:}→\n;=]*"

@rule attr = obname & r"::Attr"p & r"{"p & type & r"}"p

@rule hom = obname & r"::"p & homexp
@rule homexp = obname & r"→" & obname & EOL

@rule EOL = "\n" , ";"
@rule nonclosing = r"[^}]"p
@rule type = nonclosing[*]
@rule name = r"[^{]*"
@rule int = r"\d+"
@rule range = int & r":"p & int
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

# Now we can try parsing some basic examples

value("1")
key("id")
pair("id = 1,")
finpair("next = 3")
row("id = 1, next = 3\n")

# We can work our way up to a full ACSet instance.

instance("instance: DDS{
State{
  id = 1, next = 3
  id = 2, next = 2
  id = 3, next = 1
}
}")

range("1:5")
obname("V")
leaftable("V=1:5")

# Our grammar isn't aware of the ACSet schema, so it will parse an instance of any schema.

instance("instance: Graph{
V=1:3
E{
    id = 1, src = 1, tgt = 3
    id = 2, src = 2, tgt = 3
}
}")

# Here are some unit tests

@testset "DDS" begin
  @test obname("State") != nothing
  @test ob("State::Ob") != nothing
  @test obline("State::Ob\n") != nothing
  @test homexp("S→S\n") != nothing
  @test hom("next::S→S\n") != nothing
  @test body("{State::Ob\nnext::S→S\n}") != nothing
  @test name("DDS") != nothing
  @test schemadef("schema: DDS{
State::Ob
}") != nothing
  @test grammar("schema: DDS{State::Ob\n}") != nothing
end

# And testing on our graph schema.

@testset "Graph Schema" begin
@test grammar("schema: Graph{ A }") == nothing
@test obname("V") != nothing
@test homexp("V→V") == nothing
@test homexp("V→V\n") != nothing
@test homexp("V → V\n") != nothing

@test grammar("schema: Graph{
  x::Ob
  f::x→x
  g::x→x
  y::Ob
}") != nothing

@test grammar("schema: Graph{
  V::Ob
  E::Ob
  src::E→V
  tgt::E→V
}") != nothing

@test grammar("schema: Circuit{
  Wire::Ob
  Gate₁::Ob
  Gate₂::Ob
  Opcode::Attr{String}
  input¹::Gate₁ → Wire
  output¹::Gate₁ → Wire
  input²₁::Gate₂ → Wire
  input²₂::Gate₂ → Wire
  output²::Gate₂ → Wire
}") != nothing
end

body("{V::Ob\n}")
body("{V::Ob\nE::Ob\n")
homexp("E→V\n")
hom("src::E → V\n}")

# Our grammar can read both schemas

grammar("schema: Graph{
  V::Ob
  E::Ob
  src::E→V
  tgt::E→V
}")

grammar("schema: ReactionNetwork{
  S::Ob
  T::Ob
  I::Ob
  O::Ob
  ℝ::Data
  is::I → S
  it::I → T
  os::O → S
  ot::O → T
  rate::T → ℝ
  pop::S → ℝ
}")

# And instances of that schema.

instance("instance: ReactionNetwork{
S=1:3
T=1:2
I{
    is = 1, it = 1
    is = 2, it = 1
    is = 2, it = 2
}
O{
    is = 2, it = 1
    is = 2, it = 1
    is = 3, it = 3
}
}")