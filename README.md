# splitfuns

This is a proof of concept of communicating information between different
modules using template Haskell. To show this off we implement a system that
allows users to define functions across multiple files. You can define a clause
in one file and another clause in another file and then combine them in a third
file.

In this repository you will find four source code files:

  * `TH.hs` - this file implements the main funcitonality
  * `Main.hs` - this file combines the base case with a recursive case of the
      factorial function and runs it.
  * `Base.hs` - this file defines the base case of the factorial function
  * `Third.hs` - this file defines the value `x` which is used in the base case

The `Third` module defines the value `x` which is used in the factorial
function, but that module is not imported in the `Main` module, so this shows
that missing modules are not a problem when combining the definitions.

This was developed to explore the design space of implementing the UUAG compiler
using template Haskell. Such an implementation should reduce friction for new
users and improve stability. Additionally it could allow us to use Haskell's
module system, which means that UUAG attribute grammars could be published to
Hackage and reused by other people.
