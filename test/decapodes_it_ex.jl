
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

import Decapodes: recognize_types, make_sum_mult_unique!

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


# NOTE: the var in a Judgement in decapodes.it is just a Symbol. It does not have a name field, so that was removed
# to_decapode helper functions
reduce_term!(t::decapodes.Term, d::AbstractDecapode, syms::Dict{Symbol, Int}) =
  let ! = reduce_term!
    @match t begin
      decapodes.Var(x) => begin 
        if haskey(syms, x)
           syms[x]
        else
          res_var = add_part!(d, :Var, name = x, type=:infer)
          syms[x] = res_var
        end
      end
      decapodes.Lit(x) => begin 
        if haskey(syms, x)
           syms[x]
        else
          res_var = add_part!(d, :Var, name = x, type=:Literal)
          syms[x] = res_var
        end
      end
      decapodes.App1(f, t) || decapodes.AppCirc1(f, t) => begin
        res_var = add_part!(d, :Var, type=:infer)
        add_part!(d, :Op1, src=!(t,d,syms), tgt=res_var, op1=f)
        return res_var
      end
      decapodes.App2(f, t1, t2) => begin
        res_var = add_part!(d, :Var, type=:infer)
        add_part!(d, :Op2, proj1=!(t1,d,syms), proj2=!(t2,d,syms), res=res_var, op2=f)
        return res_var
      end
      decapodes.Plus(ts) => begin
        summands = [!(t,d,syms) for t in ts]
        res_var = add_part!(d, :Var, type=:infer, name=:sum)
        n = add_part!(d, :Σ, sum=res_var)
        map(summands) do s
          add_part!(d, :Summand, summand=s, summation=n)
        end
        return res_var
      end
      # TODO: Just for now assuming we have 2 or more terms
      decapodes.Mult(ts) => begin
        multiplicands  = [!(t,d,syms) for t in ts]
        res_var = add_part!(d, :Var, type=:infer, name=:mult)
        m1,m2 = multiplicands[1:2]
        add_part!(d, :Op2, proj1=m1, proj2=m2, res=res_var, op2=Symbol("*"))
        for m in multiplicands[3:end]
          m1 = res_var
          m2 = m
          res_var = add_part!(d, :Var, type=:infer, name=:mult)
          add_part!(d, :Op2, proj1=m1, proj2=m2, res=res_var, op2=Symbol("*"))
        end
        return res_var
      end
      decapodes.Tan(t) => begin 
        # TODO: this is creating a spurious variable with the same name
        txv = add_part!(d, :Var, type=:infer)
        tx = add_part!(d, :TVar, incl=txv)
        tanop = add_part!(d, :Op1, src=!(t,d,syms), tgt=txv, op1=DerivOp)
        return txv #syms[x[1]]
      end
      _ => throw("Inline type judgements not yet supported!")
    end
  end

function eval_eq!(eq::decapodes.Eq, d::AbstractDecapode, syms::Dict{Symbol, Int}, deletions::Vector{Int}) 
  @match eq begin
    decapodes.Eq(t1, t2) => begin
      lhs_ref = reduce_term!(t1,d,syms)
      rhs_ref = reduce_term!(t2,d,syms)

      # Always let the a named variable take precedence 
      # TODO: If we have variable to variable equality, we want
      # some kind of way to check track of this equality
      ref_pair = (t1, t2)
      @match ref_pair begin
        (decapodes.Var(a), decapodes.Var(b)) => return d
        (t1, decapodes.Var(b)) => begin
          lhs_ref, rhs_ref = rhs_ref, lhs_ref
        end
        _ => nothing
      end

      # Make rhs_ref equal to lhs_ref and adjust all its incidents

      # Case rhs_ref is a Tan
      # WARNING: Don't push to deletion here because all TanVars should have a 
      # corresponding Op1. Pushing here would create a duplicate which breaks rem_parts!
      for rhs in incident(d, rhs_ref, :incl)
        d[rhs, :incl] = lhs_ref
      end
      # Case rhs_ref is a Op1
      for rhs in incident(d, rhs_ref, :tgt)
        d[rhs, :tgt] = lhs_ref
        push!(deletions, rhs_ref)
      end
      # Case rhs_ref is a Op2
      for rhs in incident(d, rhs_ref, :res)
        d[rhs, :res] = lhs_ref
        push!(deletions, rhs_ref)
      end
      # Case rhs_ref is a Plus
      # FIXME: this typeguard is a subsitute for refactoring into multiple dispatch
      if isa(d, SummationDecapode)
        for rhs in incident(d, rhs_ref, :sum)
          d[rhs, :sum] = lhs_ref
          push!(deletions, rhs_ref)
        end
      end
      # TODO: delete unused vars. The only thing stopping me from doing 
      # this is I don't know if CSet deletion preserves incident relations
      #rem_parts!(d, :Var, sort(deletions))
    end
  end
  return d
end

function SummationDecapode(e::decapodes.DecaExpr)
  d = SummationDecapode{Any, Any, Symbol}()
  symbol_table = Dict{Symbol, Int}()

  for judgement in e.context
    var_id = add_part!(d, :Var, name=judgement.var, type=judgement.dim)
    symbol_table[judgement.var] = var_id
  end

  deletions = Vector{Int}()
  for eq in e.equations
    eval_eq!(eq, d, symbol_table, deletions)
  end
  rem_parts!(d, :Var, sort(deletions))

  recognize_types(d)

  fill_names!(d)
  d[:name] = normalize_unicode.(d[:name])
  make_sum_mult_unique!(d)
  return d
end


# Convert a the DecaExpr to a SummationDecapode which is the
# combinatorial representation. The converter lives in Decapodes/src/language.jl.

d = SummationDecapode(mexpr.model)

# We want different metadata for this representation.
# The Summation prefix just means that this decapodes have
# specialized support for the handling of summation.
# The summation operator happens in physics so often,
# that you want to bake in some specialized handling to the data structure.

h = AMR.Header("","harmonic_oscillator",
  "modelreps.io/SummationDecapode",
  "A Simple Harmonic Oscillator as a Diagrammatic Equation",
  "SummationDecapode",
  "v1.0")
mpode = decapodes.ASKEMDecapode(h, d, annot)


# The syntactic representation can be serialized as JSON.
# The resulting structure is like a parse tree of the syntactic
# representation of the DecaExpr
write_json_model(mexpr)

# We could also use the JSON serialization built into Catlab
# to serialize the resulting combinatorial representation
sm_write_json_acset(mpode.model, "$(mpode.header.name)-acset")
# end


 # Can we read back the models we just wrote?
@testset "Decapodes Readback" begin
  mexpr′ = readback(mexpr)
  @test JSON3.write(mexpr) == JSON3.write(mexpr′)
end


(mexpr == jsonread(jsonwrite(mexpr), decapodes.ASKEMDeca))
