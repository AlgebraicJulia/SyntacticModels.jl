module AMR

export amr, Math, MathML, ExpressionFormula, Unit, Distribution, Observable, Expression,
 Rate, Initial, Parameter, Time,
 StandardUniform, Uniform, StandardNormal, Normal, PointMass, Undefined,
 Semantic, Header, ODERecord, ODEList, ASKEModel, Typing, 
 distro_string, amr_to_string,
 Annotation, Note, Name, Description, Grounding, Units, nomath, nounit

using Reexport
@reexport using MLStyle
@reexport using ACSets
using ACSets.ADTs
using ACSets.ACSetInterface
using StructTypes


@intertypes "../src/amr.it" module amr end

using .amr

nomath = Math("")
nounit = Unit("", nomath)

function distro_string(d::Distribution)
  @match d begin
    StandardUniform(s)   => "U(0,1)"
    Uniform(min, max) => "U($min,$max)"
    StandardNormal(s)    => "N(0,1)"
    Normal(mu, var)   => "N($mu,$var)"
    PointMass(value)  => "δ($value)"
    Undefined(s)      => "Undefined()"
  end
end

function distro_expr(d::Distribution)
  return Base.Meta.parse(distro_string(d))
end

function note_string(n::Note)
  @match n begin
    Name(n)           => "Name($n)"
    Description(d)    => "Description($d)"
    Grounding(ont, ident) => "Grounding($ont,$ident)"
    Units(e)          => "Units($e)"
  end
end

function note_expr(n::Note)
  return Base.Meta.parse(note_string(n))
end

padlines(ss::Vector, n) = map(ss) do s
  " "^n * s
end
padlines(s::String, n=2) = join(padlines(split(s, "\n"), n), "\n")

function amr_to_string(amr′)
  let ! = amr_to_string
    @match amr′ begin
      s::String                            => s
      Math(s)                          => !s
      Presentation(s)                  => "<mathml> $s </mathml>"
      u::Unit                          => !u.expression
      d::Distribution                  => distro_string(d)
      Time(id, u)                      => "$id::Time{$(!u)}\n"
      Rate(t, f)                       => "$t::Rate = $(f.expression)"
      Initial(t, f)                    => "$t::Initial = $(f.expression)"
      Observable(id, n, states, f)     => "# $n\n$id::Observable = $(f.expression)($states)\n"
      Header(name, s, d, sn, mv)   => "\"\"\"\nASKE Model Representation: $name$mv :: $sn \n   $s\n\n$d\n\"\"\""
      Parameter(t, n, d, u, v, dist)   => "\n# $n-- $d\n$t::Parameter{$(!u)} = $v ~ $(!dist)\n"
      m::amr.ACSetSpec                     => "Model = begin\n$(padlines(sprint(show, m),2))\nend"
      ODEList(l)                       => "ODE_Equations = begin\n" * padlines(join(map(!, l), "\n")) * "\nend"
      ODERecord(rts, init, para, time) => join(vcat(["ODE_Record = begin\n"], !rts , !init, !para, [!time, "end"]), "\n")
      vs::Vector{amr.Pair}                 => map(vs) do v; "$(v.first) => $(v.second)," end |> x-> join(x, "\n") 
      vs::Vector{Semantic}             => join(map(!, vs), "\n\n")
      xs::Vector                       => map(!, xs)
      Typing(system, map)              => "Typing = begin\n$(padlines(!system, 2))\nTypeMap = [\n$(padlines(!map, 2))]\nend"
      ASKEModel(h, m, s)               => "$(!h)\n$(!m)\n\n$(!s)"
      Annotation(e,t,n)                => "Annotation = $(String(e)),$(String(t)): $(note_string(n))"
    end
  end
end

block(exprs) = begin
  q = :(begin
    
  end)
  append!(q.args, exprs)
  return q
end

extract_acsetspec(s::String) = join(split(s, " ")[2:end], " ") |> Meta.parse


function amr_to_expr(amr′)
  let ! = amr_to_expr
    @match amr′ begin
      s::String                        => s
      Math(s)                          => :(Math($(!s)))
      Presentation(s)                  => :(Presentation($(!s)))
      u::Unit                          => u.expression
      d::Distribution                  => distro_expr(d)
      Time(id, u)                      => :($id::Time{$(!u)})
      Rate(t, f)                       => :($t::Rate = $(f.expression))
      Initial(t, f)                    => :($t::Initial = $(f.expression))
      Observable(id, n, states, f)     => begin "$n"; :(@doc $x $id::Observable = $(f.expression)($states)) end
      Header(name, s, d, sn, mv)       => begin x = "ASKE Model Representation: $name$mv :: $sn \n   $s\n\n$d"; :(@doc $x) end
      Parameter(t, n, d, u, v, dist)   => begin x = "$n-- $d"; :(@doc $x  $t::Parameter{$(!u)} = $v ~ $(!dist)) end
      m::amr.ACSetSpec                     => :(Model = begin $(extract_acsetspec(sprint(show, m))) end)
      ODEList(l)                       => :(ODE_Equations = $(block(map(!, l))))
      ODERecord(rts, init, para, time) => :(ODE_Record = (rates=$(!rts), initials=$(!init), parameters=$(!para), time=!time))
      vs::Vector{amr.Pair}                 => begin ys = map(vs) do v; :($(v[1]) => $(v[2])) end; block(ys) end
      vs::Vector{Semantic}             => begin ys = map(!, vs); block(ys) end
      xs::Vector                       => begin ys = map(!, xs); block(ys) end
      Typing(system, map)              => :(Typing = $(!system); TypeMap = $(block(map)))
      ASKEModel(h, m, s)               => :($(!h);$(!m);$(!s))
      Annotation(e,t,n)                => "Annotation = $(String(e)),$(String(t)): $(note_expr(n))"
    end
  end
end

optload(d, path, default=nothing) = begin
  let ! = optload
    @match path begin
      s::Symbol => !(d, string(s), default)
      s::String => get(d, s, default)
      [addr] => !(d, addr, default)
      [head, args...] => !(get(d, head, Dict()), args, default)
      _=> error("Bad recursion in optload($d, $path)")
    end
  end
end

function petrispec(dict::AbstractDict)
  findkwarg(kwarg::Symbol, d::AbstractDict, path, default=nothing) = ADTs.Kwarg(kwarg, ADTs.Value(optload(d, path, default)))
  loadstate(s) = begin
    ADTs.Statement(:S, [findkwarg(k, s, p, :nothing) for (k,p) in [(:id, "id"), (:name, "name"), (:units, ["units", "expression"])]])
  end
  states = [loadstate(s) for s in dict["states"]]
  transi = [
    ADTs.Statement(:T,
    [ADTs.Kwarg(:id, ADTs.Value(Symbol(t["id"]))), ADTs.Kwarg(:name, ADTs.Value(t["properties"]["name"])), ADTs.Kwarg(:desc, ADTs.Value(t["properties"]["description"])) ]
    ) for t in dict["transitions"]]

  inputs = [[
    ADTs.Statement(:I,
    [ADTs.Kwarg(:is, ADTs.Value(i)), ADTs.Kwarg(:it, ADTs.Value(t["id"]))]) for i in t["input"]] for t in dict["transitions"]
  ] |> Base.Flatten |> collect
  outputs = [[
    ADTs.Statement(:O,
    [ADTs.Kwarg(:os, ADTs.Value(i)), ADTs.Kwarg(:ot, ADTs.Value(t["id"]))]) for i in t["output"]] for t in dict["transitions"]
  ] |> Base.Flatten |> collect
  ADTs.ACSetSpec(:AMRPetriNet, vcat(states, transi, inputs, outputs))
end

function load(::Type{Unit}, d::AbstractDict)
  ud = get(d, "units", Dict("expression"=>"", "expression_mathml"=>""))
  u = Unit(ud["expression"], Presentation(ud["expression_mathml"]))
end

function load(::Type{Time}, t::AbstractDict)
  Time(Symbol(t["id"]), load(Unit, t))
end

function load(::Type{Rate}, r::AbstractDict)
  f = ExpressionFormula(r["expression"], Presentation(r["expression_mathml"]))
  Rate(Symbol(r["target"]), f)
end

function load(::Type{Initial}, d::AbstractDict)
  f = ExpressionFormula(d["expression"], Presentation(d["expression_mathml"]))
  Initial(Symbol(d["target"]), f)
end

function load(::Type{Distribution}, d::AbstractDict)
  @match d begin
    Dict("type"=>"StandardUniform1")           => StandardUniform("")
    Dict("type"=>"StandardNormal")             => StandardNormal("")
    Dict("type"=>"Uniform", "parameters"=>p)   => Uniform(p["minimum"], p["maximum"])
    Dict("type"=>"Uniform1", "parameters"=>p)  => Uniform(p["minimum"], p["maximum"])
    Dict("type"=>"Normal", "parameters"=>p)    => Normal(p["mu"], p["var"])
    Dict("type"=>"PointMass", "parameters"=>p) => PointMass(p["value"])
  end
end

# TODO: determine best way to handle non-specified distributions
# load(::Type{Distribution}, ::Nothing) = PointMass(missing)
load(::Type{Distribution}, ::Nothing) = Undefined("")

function load(::Type{Note}, d::AbstractDict)
  @match d begin
    Dict("type"=>"Name", "parameters"=>p) => Name(p["str"])
    Dict("type"=>"Description", "parameters"=>p) => Description(p["str"])
    Dict("type"=>"Grounding", "parameters"=>p) => Grounding(p["ontology"], p["identifier"])
    Dict("type"=>"Units", "parameters"=>p) => Units(p["expression"])
  end
end
function load(::Type{Annotation}, d::AbstractDict)
  Annotation(d["entity"], d["type"], load(Note,d["note"]))
end

function load(::Type{Parameter}, d::AbstractDict)
  u = load(Unit, d)
  # TODO: determine best way to handle non-specified distributions
  if isnothing(get(d,"distribution", nothing)) 
    tmp_dist = !isnothing(get(d,"value", nothing)) ? PointMass(d["value"]) : Undefined("")
  else
    tmp_dist = load(Distribution, d["distribution"])
  end 
  Parameter(
    Symbol(d["id"]),
    d["name"],
    d["description"],
    u,
    d["value"],
    tmp_dist
    )
end

function load(::Type{ODERecord}, d::AbstractDict)
  time = load(Time, d["time"])
  rate(x) = load(Rate, x)
  initial(x) = load(Initial, x)
  parameter(x) = load(Parameter, x)
  rates = rate.(d["rates"])
  initials = initial.(d["initials"])
  parameters = parameter.(d["parameters"])

  ODERecord(rates, initials, parameters, time)
end

function load(::Type{Header}, d::AbstractDict)
    @match d begin
      Dict("name"=>n, "schema"=>s, "description"=>d, "schema_name"=>sn, "model_version"=>mv) => Header(n,s,d,sn,mv) 
      _ => error("Information for Header was not found in $d")
    end
end

function load(::Type{Typing}, d::AbstractDict)
  @match d begin
    Dict("type_system"=>s, "type_map"=>m) => begin @show m;  Typing(convert_acsetspec_to_it(petrispec(s)), convert_pair_to_it.([x[1]=> x[2] for x in m])) end 
    _ => error("Typing judgement was not properly encoded in $d")
  end
end

function load(::Type{ASKEModel}, d::AbstractDict)
  hdr = load(Header, d)
  hdr.schema_name == "petrinet" || error("only petrinet models are supported")
  mdl = convert_acsetspec_to_it(petrispec(d["model"]))
  sem = []
  if haskey(d["semantics"], "ode")
    push!(sem, load(ODERecord, d["semantics"]["ode"]))
  end
  if haskey(d["semantics"], "typing")
    push!(sem, load(Typing, d["semantics"]["typing"]))
  end
  ASKEModel(hdr, mdl, sem)
end

using MLStyle.Modules.AST

function load(::Type{Time}, ex::Expr)
  @matchast ex quote
    $a::Time{} => Time(a, Unit("", Math("")))
    $a::Time{$b} => Time(a, load(Unit, b))
    _ => error("Time was not properly encoded as Expr $ex")
  end
end

function load(::Type{Unit}, ex::Union{Symbol, Expr})
  Unit(string(ex), Math(""))
end

function load(::Type{ExpressionFormula}, ex::Expr)
  ExpressionFormula(string(ex), Math(string(ex)))
end

function load(::Type{Rate}, ex::Expr)
  @matchast ex quote
    ($a::Rate = $ex) => Rate(a, load(ExpressionFormula, ex))
    ($a::Rate{$u} = $ex) => Rate(a, load(ExpressionFormula, ex))
    _ => error("Rate was not properly encoded as Expr $ex")
  end
end

function load(::Type{Initial}, ex::Expr)
  @matchast ex quote
    ($a::Initial = $ex) => Rate(a, load(ExpressionFormula, ex))
    ($a::Initial{$u} = $ex) => Rate(a, load(ExpressionFormula, ex))
    _ => error("Rate was not properly encoded as Expr $ex")
  end
end

function docval(exp::Expr)
  s, ex = @match exp begin
    Expr(:macrocall, var"@doc", _, s, ex) => (s,ex)
    _ => error("Could not match documented value in $exp")
  end
  name, desc = split(s, "--")
  return strip(name), strip(desc), ex
end

function load(d::Type{Distribution}, ex::Expr)
  # println(ex)
  @matchast ex quote
    U(0,1)        => StandardUniform("")   
    U($min,$max)  => Uniform(min, max) 
    N(0,1)        => StandardNormal("")
    N($mu,$var)   => Normal(mu, var)   
    δ($value)     => PointMass(value)  
    Undefined()     => Undefined("")  
    _ => error("Failed to find distribution in $ex")
  end
end

function load(::Type{Parameter}, ex::Expr)
  name, desc, ex = docval(ex)
  id, u, val, dist = @matchast ex quote
    ($id::Parameter{} = ($val ~ $d))    => (id, nounit, val, load(Distribution, d))
    ($id::Parameter{$u} = ($val ~ $d))  => (id, load(Unit, u), val, load(Distribution, d))
    # TODO: determine best way to handle non-specified distributions
    ($id::Parameter{} = $val)           => (id, nounit, val, Undefined("")) # PointMass(missing), PointMass(NaN)
    ($id::Parameter{$u} = $val)         => (id, load(Unit, u), val, Undefined(""))
  end
  Parameter(id, name, desc, u, val, dist)
end

function load(::Type{ODEList}, ex::Expr)
  map(ex.args[2].args) do arg
    try 
      return load(Rate, arg)
    catch ErrorException 
      try 
        return load(Initial, arg)
      catch ErrorException 
        try 
          return load(Parameter, arg)
        catch ErrorException 
          try 
            return load(Time, arg)
          catch
            return nothing
          end
        end
      end
    end
  end |> x->filter(!isnothing, x) |> ODEList
end

function load(::Type{Header}, ex::String)
  hdr, schema, _, desc, _ = split(ex, "\n")
  hdr, schema_name = split(hdr, "::")
  _, hdr = split(hdr, ":")
  name, version = split(hdr, "@")
  Header(strip(name), strip(schema), strip(desc), strip(schema_name), strip(version))
end

function load(::Type{ADTs.ACSetSpec}, ex::Expr)
  let ! = x->load(ADTs.ACSetSpec, x)
    @match ex begin
      Expr(:(=), name, body) => @match body.args[2] begin
        Expr(:(=), type, body) => acsetspec(type, body)
      end
      Expr(:block, lnn, body) => !(body)
      _ => ex
    end
  end
end

function convert_val_to_it(v)
  if typeof(v)==QuoteNode v=v.value end
  if typeof(v)==Symbol
    it = amr.ValSymbol(v)
  elseif typeof(v)==String
    it = amr.ValString(v) 
  else
    error("Value type not found: $v") 
  end
  it
end

convert_pair_to_it(p) = amr.Pair(p[1],p[2])

function convert_acsetspec_to_it(x::ADTs.ACSetSpec)
  y = amr.ACSetSpec(Symbol(x.acstype),[])
  for (ii, stmt) in enumerate(x.body)
    # println(ii," ",stmt)
    push!(y.body,amr.Statement(stmt.table,[]))
    for (jj, arg) in enumerate(stmt.element)
      # println(jj," ",arg)
      if length(propertynames(arg))==2
        # println(amr.Kwarg(arg._1,amr.Value(arg._2._1)))
        push!(y.body[ii].element,amr.Kwarg(arg._1,convert_val_to_it(arg._2._1)))
      elseif length(propertynames(arg))==1
        # println(amr.Value(arg._1))
        push!(y.body[ii].element,convert_val_to_it(arg._1))
      else
        error("Bad Arg in ACSetSpec, stmt $ii, arg $jj: $arg")
      end
    end
  end
  y
end

function load(::Type{amr.ACSetSpec}, ex::Expr)
  convert_acsetspec_to_it(load(ADTs.ACSetSpec, ex))
end

function load(::Type{Typing}, ex::Expr)
  let !(x) = load(Typing, x)
    @match ex begin
      Expr(:(=), :Model, body) => load(amr.ACSetSpec, ex)
      Expr(:(=), :TypeMap, list) => !list
      Expr(:(=), :Typing, body) => Typing(!(body.args[2]), !(body.args[4]))
      Expr(:vect, args...) => map(args) do arg
        @match arg begin
          Expr(:call, :(=>), a, b) => amr.Pair(a,b)
          _ => error("The type map is expected to be pairs defined with a => fa. Got $arg")
        end
      end
      _ => error("Could not processing Typing assignment from $ex")
    end
  end
end

function load(::Type{ASKEModel}, ex::Expr)
  elts = map(ex.args) do arg
    @match arg begin
      Expr(:macrocall, var"@doc", _, s, ex) => (load(Header, s), load(amr.ACSetSpec, ex))
      Expr(:(=), :ODE_Record, body) => load(ODEList, arg)
      Expr(:(=), :ODE_Equations, body) => load(ODEList, arg)
      Expr(:(=), :Typing, body) => load(Typing, arg)
      _ => arg 
      end
  end
  ASKEModel(elts[2][1], elts[2][2], [elts[4], elts[6]])
end

end # module end