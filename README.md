ppx\_demo
=========

This preprocessor has two different modes of action. They will be defined below.

You might want this ppx if you have a program which acts as documentation. For
example, the Bonsai devs have a collection of examples that demonstrate how to
use various UI components. Viewing those examples in a browser shows the result
of the code, but not the code itself, which means that if someone wants to see
how much code a particular example takes, they need to navigate in their text
editor to the example's source file. This ppx makes it possible to easily
include source for an example directly within the example itself.

## %demo

This version of the preprocessor expands an arbitrary expression into a tuple
containing the source code for the expression along with the expression itself.
It is only usable in place of module expressions or expressions. Look at the tests in `test.ml`
for some examples.

## %demo_hoist

This version of the preprocessor will not change the output value of the expression. Instead,
it retrieves the source code and hoists it into a module named `Ppx_demo_hoist`. This makes
it a lot more ergonomic to write documentation, as you have access to the source code string
even within the expression that it describes.

The syntax for `%demo_hoist` is a bit different from `%demo`. Please look at the top-level
tests as well as the `test_hoist.ml` file for examples.
