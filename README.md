# calculus-parser

Current plan:
1. Choose data types, defined in src/Expression.hs
2. Write a parser for the our initial data types, in src/Parser.hs
3. Define data types for laws we can use to solve the inputs
4. Figure out how to reason with our defined laws
5. Add a special feature

## Derivative Reasoning Update

At this point we believe that most the derivative reasoning is working appropriately. Our one concern is how we handle the matching of the constant law. In the `matchExprs` functions in `Calculation.hs` we basically hard code our constants law to pattern match on constN variables and termFunc variables applied to an empty list. We figured there must be a better way to do this. We were having trouble with this law because the pattern matching is less trivial since the matching depends on more than just the type but the actual variable contents.

## Testing:

To test run `stack run` and then an expression such as `deriv(x, x+y)`
