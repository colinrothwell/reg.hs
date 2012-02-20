reg.hs
======

Colin Rothwell's Register Machine Interpreter
---------------------------------------------

### Building

You should just need GHC. I've tested it with 7.03, YMMV. You just need to do:

`ghc reg.hs`

And the `reg` executable will appear in the pwd.

### Running

`./reg <file> <registers>`

Where file is a register machine program, and registers is a list of register
names to output, and initial register values in the form `<name>=<value`. There
shouldn't be a space between name, =, and value.

### File Syntax

Runs programs written for a Minksy style register machine. It should be started
with a text file consisting of one instruction per line. 

Three instructions are supported.

Any line may be prefixed with a string label (with no spaces) followed by a
colon. This may be used as a brancy destination, hiding integer branches.

`R` denotes a register (an arbitrary string: all registers are intialised to 0)
and `B` denotes a branch to a label (checked first) or an integer line number.

* `R+ [B]` Adds 1 to `R`, and branches to `B`. If `B` is omitted, branches to
   the next instruction.
* `R- [B] B'` Subtracts 1 from R and branches to `B`, or the next instruction if
  `B` is omitted, if it is greater than 0. If it is equal to zero, it branches
   to `B'`.
*  `halt` Case insensitive: halts execution of the program.

Do not expect useful error messages!
