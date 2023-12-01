
# using SyntacticModels

using ACSets
using ACSets.InterTypes
using Test
using OrderedCollections
import JSON
import JSON3
# import JSONSchema

# include("src/SyntacticModels.jl")

# using .SyntacticModels
# # using .SyntacticModels.ASKEMDecapodes
using Decapodes


using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes

# using ..SyntacticModelsBase


@intertypes "../src/amr.it" module AMR end

using .AMR



@intertypes "../src/decapodes.it" module decapodes
  import ..AMR
end

using .decapodes



h = AMR.Header("", "harmonic_oscillator",
  "modelreps.io/DecaExpr",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "DecaExpr",
  "v1.0")

# The easiest way to write down a DecaExpr is in our DSL and calling the parser.
dexpr = my_parse_decapode(quote
  X::Form0{Point}
  V::Form0{Point}

  k::Constant{Point}

  ∂ₜ(X) == V
  ∂ₜ(V) == -1*k*(X)
end
)

annot = [AMR.Annotation(:X,:Form0,AMR.Name("The X variable."))]

# Bundle the DecaExpr with the header metadata.
mexpr = decapodes.ASKEMDecaExpr(h, dexpr, annot)



my_term(s::Symbol) = decapodes.Var(normalize_unicode(s))
my_term(s::Number) = decapodes.Lit(Symbol(s))

my_term(expr::Expr) = begin
    @match expr begin
        #TODO: Would we want ∂ₜ to be used with general expressions or just Vars?
        Expr(:call, :∂ₜ, b) => decapodes.Tan(decapodes.Var(b)) 
        Expr(:call, :dt, b) => decapodes.Tan(decapodes.Var(b)) 

        Expr(:call, Expr(:call, :∘, a...), b) => decapodes.AppCirc1(a, my_term(b))
        Expr(:call, a, b) => decapodes.App1(a, my_term(b))

        Expr(:call, :+, xs...) => decapodes.Plus(my_term.(xs))
        Expr(:call, f, x, y) => decapodes.App2(f, my_term(x), my_term(y))

        # TODO: Will later be converted to Op2's or schema has to be changed to include multiplication
        Expr(:call, :*, xs...) => decapodes.Mult(my_term.(xs))

        x => error("Cannot construct term from  $x")
    end
end

function my_parse_decapode(expr::Expr)
    stmts = map(expr.args) do line 
        @match line begin
            ::LineNumberNode => missing
            # TODO: If user doesn't provide space, this gives a temp space so we can continue to construction
            # For now spaces don't matter so this is fine but if they do, this will need to change
            Expr(:(::), a::Symbol, b::Symbol) => decapodes.Judgement(decapodes.Var(a).name, b, :I)
            Expr(:(::), a::Expr, b::Symbol) => map(sym -> decapodes.Judgement(decapodes.Var(sym).name, b, :I), a.args)

            Expr(:(::), a::Symbol, b) => decapodes.Judgement(decapodes.Var(a).name, b.args[1], b.args[2])
            Expr(:(::), a::Expr, b) => map(sym -> decapodes.Judgement(decapodes.Var(sym).name, b.args[1], b.args[2]), a.args)

            Expr(:call, :(==), lhs, rhs) => decapodes.Eq(my_term(lhs), my_term(rhs))
            _ => error("The line $line is malformed")
        end
    end |> skipmissing |> collect
    judges = []
    eqns = []
    foreach(stmts) do s
      @match s begin
        ::decapodes.Judgement => push!(judges, s)
        ::Vector{decapodes.Judgement} => append!(judges, s)
        ::decapodes.Eq => push!(eqns, s)
        _ => error("Statement containing $s of type $(typeof(s)) was not added.")
      end
    end
    decapodes.DecaExpr(judges, eqns)
end

(mexpr == jsonread(jsonwrite(mexpr), decapodes.ASKEMDeca))


#=
@intertypes "simpleast.it" module simpleast end

using .simpleast

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

s = jsonwrite(t)

@test s isa String

@test jsonread(s, Term) == t

generate_jsonschema_module(simpleast, ".")

simpleast_schema = JSONSchema.Schema(read("simpleast_schema.json", String))

@test JSONSchema._validate(simpleast_schema, JSON.parse(s), "Term") === nothing

@intertypes "model.it" module model
  import ..simpleast
end

using .model

e = Equation(t, t)

m = Model([:x], [e])

@test testjson(m)

@intertypes "wgraph.it" module wgraph end

using .wgraph

g = EDWeightedGraph()
add_parts!(g, :V, 2)
add_part!(g, :E, src=1, tgt=2, weight=EdgeData(:mass_ave, 42))

@test testjson(m)

generate_jsonschema_module(wgraph, ".")

wgraph_schema = JSONSchema.Schema(read("wgraph_schema.json", String))

@test JSONSchema._validate(wgraph_schema, JSON.parse(jsonwrite(g)), "EDWeightedGraph") === nothing
=#