# calculus-parser

## Current plan:
1. Choose data types, defined in src/Expression.hs
2. Write a parser for the our initial data types, in src/Parser.hs
3. Define data types for laws we can use to solve the inputs
4. Figure out how to reason with our defined laws
5. Add a special feature

## Running:
Execute the program with `stack run`, which takes an optional command line argument: the path for a text file containing laws. The default file is [laws.txt](./laws.txt). There should be one law per line and no separators. Invalid laws will throw a parsing error.

After the laws are read and parsed, you will be prompted to enter a calculus expression you want solved using those laws. There are several guidelines for formatting expressions:
1. Variables begin with a single letter, such as `x`, `y1`, but not `alpha`
2. Functions begin with two letters, and can contain 0+ arguments, such as `sin(x)`, `deriv(x, x+1)`, or `lambda`, but not `a(x)`.
3. The derivative of a function with greater than 0 arguments is always considered a function of the variable the derivative is with respect to. Thus, the expression `deriv(x, sin(y))` is already in its simplest form, while `deriv(x, lambda)` will reduce to 0. If you would like `sin(y)` from the first example to be treated as a constant and reduce to 0 as well, then don't include the arguments as they are irrelevant to the calculation: simply `deriv(x, sin)`.

## Testing
Test the program with `stack test`, which takes an optional command line argument: the path for a text file containing laws. The default file is [laws.txt](./laws.txt). There should be one law per line, no commas. Invalid laws with throw a parsing error.

First, the program will try to parse the laws from the specified file. It will then filter out all laws with expressions containing untestable functions, and attemp to test the remaining laws using leancheck. Finally, it will run our static tests, which test expression parsing and calculus solver reasoning.

## TODO 
* pretty printing calculations
* advanced feature:
  * `a(b+c) = ab + ac`
  * `a(b-c) = ab - ac`
  * `a^(b+c) = a^b * a^c`
* minor parsing fixes:
  * `parse expr 1.54`
  * `parse expr 2sin(x)`
  * `parse a(b+c)`
  * more operators?
* sort laws to apply in a more logical order?
* expand tests?
* combine like terms?