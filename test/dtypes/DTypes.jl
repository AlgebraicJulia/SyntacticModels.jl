module TestDTypes

using SyntacticModels.DTypes
using Test

include(as_dtypes(), "ast.dtypes")

@test dtype(Term) isa DType

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

@test jsonwrite(t) isa String
@test jsonread(jsonwrite(t), Term) == t

end
