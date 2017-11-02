# Homework #6



- #####Functions with Multiple Arguments

	Start with the FWAE interpreter, and extend the implementation to support any number of arguments to a function (including zero), and any number of arguments (including zero) in a function application:
    
        <FWAE>  ::= <number>
              | {+ <FWAE> <FWAE>}
              | {- <FWAE> <FWAE>}
              | {with {<id> <FWAE>} <FWAE>}
              | <id>
              | {<FWAE> <FWAE>*}
              | {fun {<id>*} <FWAE>}

	Since you must change the FWAE datatype, and since different people may change it in different ways, you must provide a parse function this time, which accepts a quoted expression and produces an FWAE value.
	For parsing, assume that any symbol other than '+, '-, 'with, or 'fun can be an identifier. At run-time, a new error is now possible: function application with the wrong number of arguments. Your interp function should detect the mismatch and report an error that includes the words “wrong arity”. A function would be ill-defined if two of its argument <id>s were the same. To prevent this problem, your parse function should detect this problem and reports a “bad syntax” error. For example, (parse '{fun {f x x} x}) should report a “bad syntax” error, while (parse '{fun {f x y} x}) should produce a fun value. Remember that the PLAI language provides the useful function map: map takes a function and a list, and applies the function to each element in the list, returning a list of results. For example, if sexps is a list of S-expressions to parse, (map parse sexps) produces a list of FWAEs by parsing each S-expression.
	You can decide whether to use deferred substitution.

- #####Adding Records

	Extend your interpreter to support the construction of records with named fields, and to support field selection from a record:

         <FWAE> ::= ...
              | {record {<id> <FWAE>}*}
              | {getField <FWAE> <id>}

	Your revised parse should check that each <id> is distinct in a record expression. If there are duplicates, the error messages should include the words “duplicate fields”.
Adding records means that the language now has three kinds of values: numbers, functions, and records. At run-time, an error may occur because a record is misused as a number, a number is supplied to getField, or a record supplied to getField does not have the named field. Your error message for the last case should include the words “no such field”, otherwise you can make up your own error messages (or just let primitive error checking handle problems, such as trying to add a record to a number).
Extend parse to support record and getField expressions.
Since records are now values, the result of interp can be a record instead of a number or a function. For homework purposes, we don't want to nail down the representation of a record result, because there are many choices. The examples below therefore use run, which is a wrapper on interp that just returns a number if interp produces a number, and it returns the symbol 'function if interp produces a function value, but it returns the symbol 'record if interp produces a record value.
Note that the ids in getField and record expressions are not bound by with or function parameters. They are more like the “x” in “o.x” expressions in C and Java.
Hint: change the result type of interp so that it retuns a new define-type that represents values, with two alternatives (numV n) for some number n or a record expression (however you chose to represent them). Note that this also affects your subst function's input (since its input is interp's output in the example code (of course, your rewritten one may not do that).

	Some examples:
    
        (test (run "{record {a 10} {b {+ 1 2}}}")
              'record)
        (test (run "{getField {record {a 10} {b {+ 1 2}}} b}")
              3)
        (test/exn (run "{getField {record {b 10} {b {+ 1 2}}} b}")
                  "duplicate fields")
        (test/exn (run "{getField {record {a 10}} b}")
                  "no such field")
        (test (run "{with {g {fun {r} {getField r c}}}
                          {g {record {a 0} {c 12} {b 7}}}}")
              12)
        (test (run "{getField {record {r {record {z 0}}}} r}")
              'record)
        (test (run "{getField {getField {record {r {record {z 0}}}} r} z}")
              0)
        (test/exn (run "{record {z {getField {record {z 0}} y}}}")
                  "no such field")
        (test (run "{with {f {fun {a b} {+ a b}}}
                          {with {g {fun {x} {- x 5}}}
                                {with {x {f 2 5}} {g x}}}}") 2)
        (test (run "{with {f {fun {x y} {+ x y}}} {f 1 2}}") 3)
        (test (run "{with {f {fun {} 5}}
                          {+ {f} {f}}}") 10)
        (test (run "{with {h {fun {x y z w} {+ x w}}}
                          {h 1 4 5 6}}") 7) 
        (test (run "{with {f {fun {} 4}}
                          {with {g {fun {x} {+ x x}}}
                                {with {x 10} {- {+ x {f}} {g 4}}}}}") 6)
        (test (run "{record {a 10} {b {+ 1 2}}}") 'record)
        (test (run "{getField {record {r {record {z 0}}}} r}") 'record)
        (test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
        (test (run "{with {x 3} {with {y 5} {getField {record {a x} {b y}} a}}}") 3)
        (test (run "{with {f {fun {a b} {+ {getField a a} b}}}
                          {with {g {fun {x} {+ 5 x}}}
                                {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}") 17)
        (test (run "{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}}
                          {getField {f 1 2 3 4 5} c}}") 3)
        (test (run "{with {f {fun {a b c} {record {a a} {b b} {c c}}}}
                          {getField {f 1 2 3} b}}") 2)
        (test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                          {getField {f 1 2 3} y}}") 2)
        (test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                          {getField {f 1 2 3} d}}") 2)
        (test (run "{with {f {fun {x} {+ 5 x}}}
                          {f {getField {getField {record {a {record {a 10} {b {- 5 2}}}} {b {getField {record {x 50}} x}}} a} b}}}") 8)
        (test (run "{getField {record {a 10} {b {+ 1 2}}} b}") 3)
        (test (run "{getField {record {r {record {z 0}}}} r}") 'record)
        (test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
        (test (run "{record {a 10}}") 'record)
        (test (run "{getField {record {a 10}} a}") 10)
        (test (run "{getField {record {a {+ 1 2}}} a}") 3)
        (test (run "{fun {x} x}") 'function)
        (test (run "{getField {record {a {record {b 10}}}} a}") 'record)
        (test (run "{getField {getField {record {a {record {a 10}}}} a} a}") 10)
        (test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} a}") 10)
        (test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} b}") 20)
        (test (run "{+ {getField {record {a 10}} a} {getField {record {a 20}} a}}") 30)
        (test (run "{+ {getField {record {a 10}} a} {getField {record {a 20}} a}}") 30)
        (test (run "{record {a 10}}") 'record)
        (test (run "{record {a {- 2 1}}}") 'record)
        (test (run "{getField {record {a 10}} a}") 10)
        (test (run "{getField {record {a {- 2 1}}} a}") 1)
        (test (run "{getField {record {a {record {b 10}}}} a}") 'record)
        (test (run "{getField {getField {record {a {record {a 10}}}} a} a}") 10)
        (test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} a}") 10)
        (test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} b}") 20)
        (test (run "{getField {record {r {record {z 0}}}} r}") 'record)
        (test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
        (test (run "{with {y {record {x 1} {y 2} {z 3}}} {getField y y}}") 2)
        (test (run "{with {y {record {x 1} {y 2} {z 3}}} {getField y z}}") 3)
        (test (run "{record {a 10} {b {+ 1 2}}}") 'record)
        (test (run "{getField {record {a 10} {b {+ 1 2}}} b}") 3)
        (test (run "{with {g {fun {r} {getField r c}}}
                          {g {record {a 0} {c 12} {b 7}}}}") 12)
        (test (run "{getField {record {r {record {z 0}}}} r}") 'record)
        (test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)