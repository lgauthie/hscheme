Hscheme
=======
A toy implementation of scheme written in Haskell for entertainment and
learning purposes.

The overall structure is based partially on the write yourself a scheme tutorial
[here](http://en.wikibooks.org/wiki/Category:Write_Yourself_a_Scheme_in_48_Hours).
Though the control flow of the program is similar, this implementation has a
far richer feature set.

Features
--------
- Fully non-backtracking recursive descent parser
- Full parser support for all scheme types including a full numerical hierarchy
- Full support for all numerical procedures
- If conditionals
- Support for Most scheme IO functionality
- Lambdas, recursion, and closures
- No type coercion, trying to add a float and an integer will raise a type
  error
- Probably many more I have forgotten!

Notable differences from Scheme
===============================
- Chars are specified with only a leading back slash (`\c`) instead of (`#\c`)
  Clojure uses this convention as well.
- Vectors are specified using square brackets (`[` `]`) instead of a hash
  followed by standard list notation. This notation is also used in Clojure.
- car and cdr have been replaced with head and tail.

Installing
==========

Dependencies
------------
- A Haskell compiler
    + tested with GHC, but uses no language extensions so should also compile
      with Hugs.
- libreadline
    + This library should be available on most \*nix systems, and there exists
      an installer for windows.

Steps
-----
- `cd src`
- `cabal install readline`
- `make`

If all goes well you are done!

TODO
====

Primitive Functions
-------------------
+ Add equive eq and equ primitive functions -- not realy needed.
+ Integer division should return a rational number
+ Char procedures missing
    - procedure:  `(char=? char1 char2)`
    - procedure:  `(char<? char1 char2)`
    - procedure:  `(char>? char1 char2)`
    - procedure:  `(char<=? char1 char2)`
    - procedure:  `(char>=? char1 char2)`
    - procedure:  `(char->integer char)`
    - procedure:  `(integer->char n)`
+ String procedures missing
    - procedure:  `(make-string k)`
    - procedure:  `(make-string k char)`
    - procedure:  `(string-ref string k)`
    - procedure:  `(string-set! string k char)`
+ Vector procedures missing
    - procedure:  `(make-vector k)`
    - procedure:  `(make-vector k fill)`
+ Control Features missing
    - procedure:  `(procedure? obj)`
    - procedure:  `(call-with-current-continuation proc)`
    - procedure:  `(call-with-values producer consumer)`
    - procedure:  `(values obj ...)`
+ Eval features missing
    - procedure:  `(eval expression environment-specifier)`
+ Input features missing
    - procedure:  `(read-char)`
    - procedure:  `(read-char port)`
    - procedure:  `(peek-char)`
    - procedure:  `(peek-char port)`
    - procedure:  `(eof-object? obj)`
    - procedure:  `(char-ready?)`
    - procedure:  `(char-ready? port)`

Language Features
-----------------
+ Add macros
+ Implement cond and case expressions
    - this could be done with macros
