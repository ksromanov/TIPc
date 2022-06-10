Static analyser for Tiny Imperative language written in Ocaml
=============================================================

Since there is no intention to write a production-quality
analysis tool, I do not store and pass the location information
between the passes.

Currently implemented are:

1. Parser/lexer using Menhir and Ocamllex, see `src/parsing`.
Result of the pass is the Parsetree. Interpreted with Parsetree
interpreter.

2. ANF or three-address code representation, see `src/anf`.
The first pass, which is converting from Parsetree to ANF is
in `src/anf/anf_of_parsetree.ml`. TODO: write ANF interpreter.

3. Damas-Hindley-Milner type inference - `src/typing`.
TODO: write an intermediate representation + integrate into
interpreter.

4. Static analysis — sign analysis using monotone framework,
runs after type inference using typed-ANF language. The analysis
is limited to integers only.
