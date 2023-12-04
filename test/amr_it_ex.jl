module AMRExamples
using ..SyntacticModels.AMR
using Test
using ACSets
using ACSets.ADTs

using ACSets.InterTypes
using Test
using OrderedCollections
import JSON
import JSON3

using Reexport
@reexport using MLStyle
@reexport using ACSets
using StructTypes



@intertypes "../src/amr.it" module AMR end

using .AMR



nomath = AMR.Math("")
header = AMR.Header("","SIR", "amr-schemas:petri_schema.json", "The SIR Model of disease", "petrinet", "0.2")
model = acsetspec(:(LabelledPetriNet{Symbol}), quote
  S(label=:S)
  S(label=:I)
  S(label=:R)

  T(label=:inf)
  T(label=:rec)

  I(is=:S, it=:inf)
  I(is=:I, it=:inf)
  I(is=:I, it=:rec)

  O(os=:I, it=:inf)
  O(os=:I, it=:inf)
  O(os=:R, it=:rec)
end) 

ode = AMR.ODERecord([AMR.Rate(:inf, AMR.ExpressionFormula( "S*I*β", nomath)),
           AMR.Rate(:rec, AMR.ExpressionFormula("I*γ", nomath))],

          [AMR.Initial(:S, AMR.ExpressionFormula("S₀", nomath)),
           AMR.Initial(:I, AMR.ExpressionFormula("I₀", nomath)),
           AMR.Initial(:R, AMR.ExpressionFormula("R₀", nomath)),],

          [AMR.Parameter(:β, "β", "the beta parameter", AMR.Unit("1/(persons^2*day)", nomath), 1e-2, AMR.Uniform(1e-3, 2e-2)),
          AMR.Parameter(:γ, "γ", "the gama parameter", AMR.Unit("1/(persons*day)", nomath), 3, AMR.Uniform(1, 2e+2)),

          AMR.Parameter(:S₀, "S₀", "the initial susceptible population", AMR.Unit("persons", nomath), 300000000.0, AMR.Uniform(1e6, 4e6)),
          AMR.Parameter(:I₀, "I₀", "the initial infected population", AMR.Unit("persons", nomath), 1.0, AMR.Uniform(1, 1)),
          AMR.Parameter(:R₀, "R₀", "the initial recovered population", AMR.Unit("persons", nomath), 0.0, AMR.Uniform(0, 4)),
          ],
          AMR.Time(:t, AMR.Unit("day", nomath)))

odelist = AMR.ODEList([
  AMR.Time(:t, AMR.Unit("day", nomath)),
  AMR.Parameter(:β, "β", "the beta parameter", AMR.Unit("1/(persons^2*day)", nomath), 1e-2, AMR.Uniform(1e-3, 2e-2)),
  AMR.Rate(:inf, AMR.ExpressionFormula("S*I*β", nomath)),

  AMR.Parameter(:γ, "γ", "the gama parameter", AMR.Unit("1/(persons*day)", nomath), 3, AMR.Uniform(1, 2e+2)),
  AMR.Rate(:rec, AMR.ExpressionFormula("I*γ", nomath)), 

  AMR.Parameter(:S₀, "S₀", "the initial susceptible population", AMR.Unit("persons", nomath), 300000000.0, AMR.Uniform(1e6, 4e6)),
  AMR.Initial(:S₀, AMR.ExpressionFormula("S₀", nomath)),

  AMR.Parameter(:I₀, "I₀", "the initial infected population", AMR.Unit("persons", nomath), 1.0, AMR.Uniform(1, 1)),
  AMR.Initial(:I₀, AMR.ExpressionFormula("I₀", nomath)),

  AMR.Parameter(:R₀, "R₀", "the initial recovered population", AMR.Unit("persons", nomath), 0.0, AMR.Uniform(0, 4)),
  AMR.Initial(:R₀, AMR.ExpressionFormula("R₀", nomath)), 

  ])

amr₁ = AMR.ASKEModel(header,
  model,
  [ode]
)
