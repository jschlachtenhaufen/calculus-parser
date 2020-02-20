# calculus-parser

Current plan:
1. Choose data types, defined in src/Expression.hs
2. Write a parser for the our initial data types, in src/Parser.hs
3. Define data types for laws we can use to solve the inputs
4. Figure out how to reason with our defined laws
5. Add a special feature

## Parsing
We had an issue when parsing termOp. TermOp parser looks for two expressions with an operation. When we check for the first expression, the parser gets stuck in an infinite loop. We solved this by only checking for non termOp types as the first argument to termOp (line 45). This allows us to parse almost everything, but we can't handle the case with a left binded operation like (x+y)+z. Any suggestions?

We tested the parser using the sample inputs defined in Expressions, eg "sin(5 + x) / (deriv(x, x^2))". We could then easily compare the output of `parseTest expr input` to our expected output determined during the data type definition step.