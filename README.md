ppx\_demo
=========

This is a preprocessor that expands an arbitrary expression into a tuple
containing the source code for the expression along with the expression itself.
Look at the tests of for some examples.

You might want this ppx if you have a program which acts as documentation. For
example, the Bonsai devs have a collection of examples that demonstrate how to
use various UI components. Viewing those examples in a browser shows the result
of the code, but not the code itself, which means that if someone wants to see
how much code a particular example takes, they need to navigate in their text
editor to the example's source file. This ppx makes it possible to easily
include source for an example directly within the example itself.
