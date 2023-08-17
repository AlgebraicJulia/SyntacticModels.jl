# SyntacticModels.jl

```@meta
CurrentModule = SyntacticModels
```

`SyntacticModels.jl` is a Julia library for representing models as syntactic expressions. 

The driving example for this library is the need to interoperate models between programming languages in the DARPA ASKEM Program. The AlgebraicJulia ecosystem includes some great tools for specifying modeling languages, but they are deeply connected to the Julia language. This package aims to provide simple tools for specifying domain specific programming languages that can be used to exchange the specification of scientific models between host languages.

We heavily use the [MLStyle.jl](https://thautwarm.github.io/MLStyle.jl/latest/index.html) package for defining [Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) so you should familiarize yourself with those concepts before reading on in this documentation.
