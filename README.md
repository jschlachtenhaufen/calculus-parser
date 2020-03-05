# calculus-parser
This project is designed to solve calculus problems based on a set of pre-defined laws.

## Project Organization
The source code for the project contains three main files: `Laws.hs`, `Expressions.hs`, and `Calculation.hs`. In our `test` folder, we test the functionality of each datatype.

## High-Level Implementation Strategy
Our datatypes are set up as follows:
An expression is a mathematical formulation that we will try to simplify. It can be made up of variables, constants, functions, and operations. A law is made up of two expressions that are equal which together can be described as an equation. A step in solving an expression can be described as the name of the law applied and the resulting expression. A calculation of an expression can be described by an expression we are trying to solve, along with a list of steps.

In `Calculation.hs` we write the majority of the important code for solving calculus expressions. We take an expression and find matches of the left side of a law equation and any subexpressions of the expression. Then we apply the right side of the law equation for a match we find. This give us our steps towards a calculation.

## Special Feature
Our special feature is automated testing of custom laws described in [laws.txt](./laws.txt). After running `stack test`, the program will attempt to parse all laws in `laws.txt` and select those that are testable. A law is testable if it contains either no functions or only testable functions, which include `sin`, `cos`, `ln`, and `sqrt`. The code in [LawTests.hs](./test/LawTests.hs) will first identify all unique free variables in the expression, generate ~1000 substitutions of variables for Doubles using LeanCheck, then create a test case for each law which consists of evaluating each substitution and asserting equality. We round results to 12 decimal places to avoid errors resulting from Double imprecision.

#### Example testable laws
* `distrib+: a(b+c) = a*b + b*c`, which will pass
* `distrib^: a^(b+c) = a^b * a^c`, which will fail, giving failed substitution of `(a, 0), (b, 1), (c, -1)`
* `factor: (a-b)^2 = a^2 -2(a*b) + b^2`, which will pass
* `sqrt: sqrt(x^2) = x`, which will fail, giving failed substituion containing the first negative number tested

## Usage:
Execute the program with `stack run`, which takes an optional command line argument: the path for a text file containing laws. The default file is [laws.txt](./laws.txt). There should be one law per line and no separators. Invalid laws will throw a parsing error.

After the laws are read and parsed, you will be prompted to enter a calculus expression you want solved using those laws. There are several guidelines for formatting expressions:
1. Variables begin with a single letter, such as `x`, `y1`, but not `alpha`
2. Functions begin with two letters, and can contain 0+ arguments, such as `sin(x)`, `deriv(x, x+1)`, or `lambda`, but not `a(x)`.
3. The derivative of a function with greater than 0 arguments is always considered a function of the variable the derivative is with respect to. Thus, the expression `deriv(x, sin(y))` is already in its simplest form, while `deriv(x, lambda)` will reduce to 0. If you would like `sin(y)` from the first example to be treated as a constant and reduce to 0 as well, then don't include the arguments as they are irrelevant to the calculation: simply `deriv(x, sin)`.
4. The expression parsing supports inputs such as `2cos(x)` -> `2 * sin(x)`. This works for expressions like `a(b+c)` -> `a*b + b*c` as well, but note that `lambda(b+c)` will not distribute as it's interpreted as a function.
5. While there is no code in place for combining like terms, you can achieve similar functionality by taking advantage of the configurability of our laws. For example, by default `a + 0` will not be simplified, but by adding the law `x + 0 = x` you can accomplish this.

#### Example input expressions using laws in [laws.txt](./laws.txt)
* `deriv(x, x+y)` -> `1 + deriv(x, y)`
* `deriv(x, x*sin(x))` -> `sin(x) + cos(x) * x`
* `deriv(x, 4x^3)` -> `4x^3 * (4/4x) * 3`
* `deriv(x, (lambda + sin(x)) / (cos(x)))` -> `(cos(x) * cos(x) - (-sin(x) * (lambda + sin(x)))) / (cos(x) ^ 2)`

## Testing
Test the program with `stack test`, which will look for a file called `laws.txt` in the root directory. There should be one law per line, no commas. Invalid laws with throw a parsing error. First, the program will perform our additional feature, specified above. It will then run a number of static tests for parsing expressions and solving calculations.



