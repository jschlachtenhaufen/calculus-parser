# calculus-parser
This project is designed to solve calculus problems based on a set of pre-defined laws.

## Project Organization
The source code for the project contains three main files: `Laws.hs`, `Expressions.hs`, and `Calculation.hs`. In our `test` folder, we test the functionality of each datatype.

## High-Level Implementation Strategy
Our datatypes are set up as follows: An expression is a mathematical formulation that we will try to simplify. It can be made up of variables, constants, functions, and operations. A Law is made up of two expressions that are equal which together can be described as an equation. A step in solving an expression can be described as an expression. A calculation of an expression can be described by an expression we are trying to solve, along with a list of steps.

In `Calculation.hs` we write the majority of the important code for solving calculus expressions. We take an expression and find matches of the left side of a law equation and any subexpressions of the expression. Then we apply the right side of the law equation for a match we find. This give us our steps towards a calculation.

## Special Feature
Our special feature is to test incoming laws to make sure they are valid. If an incoming law does not contain any functions that we cannot test, e.g. deriv, then we test that law to make sure it holds. In `LawsTest.hs` we verify laws by identifying all free variables, and using LeanCheck to plug in numbers for the variables on each side. If all the test cases pass, we can assume that this law holds generally. We round the result on each side to 12 decimal places to avoid rounding errors.

## Usage:
Execute the program with `stack run`, which takes an optional command line argument: the path for a text file containing laws. The default file is [laws.txt](./laws.txt). There should be one law per line and no separators. Invalid laws will throw a parsing error.

After the laws are read and parsed, you will be prompted to enter a calculus expression you want solved using those laws. There are several guidelines for formatting expressions:
1. Variables begin with a single letter, such as `x`, `y1`, but not `alpha`
2. Functions begin with two letters, and can contain 0+ arguments, such as `sin(x)`, `deriv(x, x+1)`, or `lambda`, but not `a(x)`.
3. The derivative of a function with greater than 0 arguments is always considered a function of the variable the derivative is with respect to. Thus, the expression `deriv(x, sin(y))` is already in its simplest form, while `deriv(x, lambda)` will reduce to 0. If you would like `sin(y)` from the first example to be treated as a constant and reduce to 0 as well, then don't include the arguments as they are irrelevant to the calculation: simply `deriv(x, sin)`.

## Testing
Test the program with `stack test`, which takes an optional command line argument: the path for a text file containing laws. The default file is [laws.txt](./laws.txt). There should be one law per line, no commas. Invalid laws with throw a parsing error.

First, the program will try to parse the laws from the specified file. It will then filter out all laws with expressions containing untestable functions, and attemp to test the remaining laws using leancheck. Finally, it will run our static tests, which test expression parsing and calculus solver reasoning.


