module ASKEMDecapodes

export ASKEMDecaExpr, ASKEMDecapode, ASKEMDeca, SummationDecapode, parse_decapode

# using ..SyntacticModelsBase
using ..AMR

using StructTypes
# using Decapodes
using MLStyle

using ACSets
using ACSets.InterTypes

using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface

import Unicode

using ..AMR.amr

@intertypes "decapodes.it" module decapodes
  import ..amr
end

using .decapodes


normalize_unicode(s::String) = Unicode.normalize(s, compose=true, stable=true, chartransform=Unicode.julia_chartransform)
normalize_unicode(s::Symbol)  = Symbol(normalize_unicode(String(s)))
DerivOp = Symbol("∂ₜ")
append_dot(s::Symbol) = Symbol(string(s)*'\U0307')

term(s::Symbol) = decapodes.Var(normalize_unicode(s))
term(s::Number) = decapodes.Lit(Symbol(s))

term(expr::Expr) = begin
    @match expr begin
        #TODO: Would we want ∂ₜ to be used with general expressions or just Vars?
        Expr(:call, :∂ₜ, b) => decapodes.Tan(decapodes.Var(b)) 
        Expr(:call, :dt, b) => decapodes.Tan(decapodes.Var(b)) 

        Expr(:call, Expr(:call, :∘, a...), b) => decapodes.AppCirc1(a, term(b))
        Expr(:call, a, b) => decapodes.App1(a, term(b))

        Expr(:call, :+, xs...) => decapodes.Plus(term.(xs))
        Expr(:call, f, x, y) => decapodes.App2(f, term(x), term(y))

        # TODO: Will later be converted to Op2's or schema has to be changed to include multiplication
        Expr(:call, :*, xs...) => decapodes.Mult(term.(xs))

        x => error("Cannot construct term from  $x")
    end
end

function parse_decapode(expr::Expr)
    stmts = map(expr.args) do line 
        @match line begin
            ::LineNumberNode => missing
            # TODO: If user doesn't provide space, this gives a temp space so we can continue to construction
            # For now spaces don't matter so this is fine but if they do, this will need to change
            Expr(:(::), a::Symbol, b::Symbol) => decapodes.Judgement(decapodes.Var(a).name, b, :I)
            Expr(:(::), a::Expr, b::Symbol) => map(sym -> decapodes.Judgement(decapodes.Var(sym).name, b, :I), a.args)

            Expr(:(::), a::Symbol, b) => decapodes.Judgement(decapodes.Var(a).name, b.args[1], b.args[2])
            Expr(:(::), a::Expr, b) => map(sym -> decapodes.Judgement(decapodes.Var(sym).name, b.args[1], b.args[2]), a.args)

            Expr(:call, :(==), lhs, rhs) => decapodes.Eq(term(lhs), term(rhs))
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

###

# NOTE: the var in a Judgement in decapodes.it is just a Symbol. It does not have a name field, so that was removed
# to_decapode helper functions
reduce_term!(t::decapodes.Term, d::decapodes.AbstractDecapode, syms::Dict{Symbol, Int}) =
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

function eval_eq!(eq::decapodes.Eq, d::decapodes.AbstractDecapode, syms::Dict{Symbol, Int}, deletions::Vector{Int}) 
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
      if isa(d, decapodes.SummationDecapode)
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

function recognize_types(d::decapodes.AbstractNamedDecapode)
  unrecognized_types = setdiff(d[:type], [:Form0, :Form1, :Form2, :DualForm0,
                          :DualForm1, :DualForm2, :Literal, :Parameter,
                          :Constant, :infer])
  isempty(unrecognized_types) ||
    error("Types $unrecognized_types are not recognized.")
end

function fill_names!(d::decapodes.AbstractNamedDecapode)
  bulletcount = 1
  for i in parts(d, :Var)
    if !isassigned(d[:,:name],i) || isnothing(d[i, :name])
      d[i,:name] = Symbol("•$bulletcount")
      bulletcount += 1
    end
  end
  for e in incident(d, :∂ₜ, :op1)
    s = d[e,:src]
    t = d[e, :tgt]
    String(d[t,:name])[1] != '•' && continue
    d[t, :name] = append_dot(d[s,:name])
  end
  d
end

function make_sum_mult_unique!(d::decapodes.AbstractNamedDecapode)
  snum = 1
  mnum = 1
  for (i, name) in enumerate(d[:name])
    if(name == :sum)
      d[i, :name] = Symbol("sum_$(snum)")
      snum += 1
    elseif(name == :mult)
      d[i, :name] = Symbol("mult_$(mnum)")
      mnum += 1
    end
  end
end

function SummationDecapode(e::decapodes.DecaExpr)
  # d = SummationDecapode{Any, Any, Symbol}()
  d = decapodes.SummationDecapode{Symbol, Symbol, Symbol}()
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



end