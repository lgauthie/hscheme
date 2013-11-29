Hscheme
=======

TODO
====

Primitive Functions
-------------------
+ Add equive eq and equ primitive functions -- not realy needed.
+ Integer division should return a rational number
+ Char procedures missing
    - procedure:  (char`=?` char1 char2)
    - procedure:  (char`<?` char1 char2)
    - procedure:  (char`>?` char1 char2)
    - procedure:  (char`<=?` char1 char2)
    - procedure:  (char`>=?` char1 char2)
    - procedure:  (char`->`integer char)
    - procedure:  (integer`->`char n)
+ String procedures missing
    - procedure:  (make-string k)
    - procedure:  (make-string k char)
    - procedure:  (string-ref string k)
    - procedure:  (string-set! string k char)
+ Vector procedures missing
    - procedure:  (make-vector k)
    - procedure:  (make-vector k fill)
+ Control Features missing
    - procedure:  (procedure? obj)
    - procedure:  (call-with-current-continuation proc)
    - procedure:  (call-with-values producer consumer)
    - procedure:  (values obj ...)
+ Eval features missing
    - procedure:  (eval expression environment-specifier)
+ Input features missing
    - procedure:  (read-char)
    - procedure:  (read-char port)
    - procedure:  (peek-char)
    - procedure:  (peek-char port)
    - procedure:  (eof-object? obj)
    - procedure:  (char-ready?)
    - procedure:  (char-ready? port)

Language Features
-----------------
+ Add macros
+ Implement cond and case expressions
    - this could be done with macros
