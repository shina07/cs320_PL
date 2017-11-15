# Homework #9



- ##### Multiple Arguments

	Start with ''kcfae.rkt'' and change the interpreter to support multiple or zero arguments to a function, and multiple or zero arguments in a function call:
    
          <KCFAE> ::= ...
                  | {fun {<id>*} <KCFAE>}
                  | {<KCFAE> <KCFAE>*}
    
	As usual, your interp function should detect the mismatch between the numbers of function parameter and arguments at function calls and report an error that includes the words “wrong arity”. Note that, kcfae.rkt provides interp-expr, which takes an expression, interprets it with an empty substitution, and produces either a number or 'function.

	Assume that each argument <id> is distinct for a fun expression. All continuations still accept a single argument. Example:
    
      (test (run "{{fun {x y} {- y x}} 10 12}") 2)
      (test (run "{fun {} 12}") 'function)
      (test (run "{fun {x} {fun {} x}}") 'function)
      (test (run "{{{fun {x} {fun {} x}} 13}}") 13)
      (test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)

- ##### Improving Sequences

	Generalize seqn to allow one or more sub-expressions, instead of exactly two sub-expressions:

        <BFAE>  ::= ...
                 | {seqn <BFAE> <BFAE>*}
    
    Example:

         (test (interp (parse '{{fun {b}
                                  {seqn
                                   {setbox b {+ 2 {openbox b}}}
                                   {setbox b {+ 3 {openbox b}}}
                                   {setbox b {+ 4 {openbox b}}}
                                   {openbox b}}}
                                 {newbox 1}})
                        (mtSub)
                        (mtSto))
                (v*s (numV 10)
                     (aSto 1 (numV 10) (mtSto))))

	**Add your own tests!**
    
    
- ##### Exceptions
 
	Extend your language to support catching and throwing exceptions:

          <KCFAE> ::= ...
                  | {try <KCFAE> catch <KCFAE>}
                  | {throw}
                 
	The try … catch form evaluates its first <KCFAE> and returns its value — unless evaluating the first <KCFAE> throws an exception by evaluating {throw}, in which case the result of the try expression is the result of the second <KCFAE>. Of course, catching the exception (and starting to evaluate the second <KCFAE>) means that the throw is not seen by any enclosing try.

	The result is undefined if throw is used without a dynamically enclosing catch.

	Examples:
        (test (run "{try 7 catch 8}")
            7)
        (test (run "{try {throw} catch 8}")
            8)
        (test (run "{try {+ 1 {throw}} catch 8}")
            8)
        (test (run "{{fun {f} {try {f 3} catch 8}}
                   {fun {x} {throw}}}")
            8)
        (test (run "{try {try {throw} catch 8} catch 9}")
            8)
        (test (run "{try {try {throw} catch {throw}} catch 9}")
            9)
        (test (run "{try {try 7 catch {throw}} catch 9}")
            7)
        (test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}}
                   {fun {x} {throw}}}")
        8)

        ; multiple arguments (5)
        (test (run "{{fun {x y} {- y x}} 10 12}") 2)
        (test (run "{fun {} 12}") 'function)
        (test (run "{fun {x} {fun {} x}}") 'function)
        (test (run "{{{fun {x} {fun {} x}} 13}}") 13)
        (test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)

        ; exceptions (35)
        (test (run "{+ {withcc k {k 5}} 4}" ) 9)
        (test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}") 55) ; recursive function
        (test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}") 100) ; exit from recursive function using continuation
        (test (run "{withcc k {- 0 {k 100}}}" ) 100)
        (test (run "{withcc k {k {- 0 100}}}" ) -100)
        (test (run "{withcc k {k {+ 100 11}}}" ) 111)
        (test (run "{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}" ) 0)
        (test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)
        (test (run "{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}") 20)
        (test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}") 110) ; exit from recursive function using try-catch
        (test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}") 54) ; equal? for multiple recursive try-catch
        (test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}") 2)
        (test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}") 20110464) ; recursive try-catch throwing (1)
        (test (run "{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}") 0)
        (test (run "{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}") 8)
        (test (run "{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}") 89)
        (test (run "{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}") 11)
        (test (run "{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}") 5)
        (test (run "{+ {try {- 10 {throw}} catch 3} 10}") 13)
        (test (run "{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}") 54)
        (test (run "{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}") 10)
        (test (run "{try {- 0 {throw}} catch 5}") 5)
        (test (run "{try {if0 {throw} 3 4} catch 5}") 5)
        (test (run "{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}") -1)
        (test (run "{try {try {throw} catch {throw}} catch 9}") 9)
        (test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
        (test (run "{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
        (test (run "{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}") 5)
        (test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}") 10)
        (test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}") 42)
        (test (run "{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}") 3)
        (test (run "{withcc esc {try {+ {throw} {esc 3}} catch 4}}") 4)
        (test (run "{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}") 15)
        (test (run "{try {withcc x {+ {x 1} {throw}}} catch 0}") 1)
        (test (run "{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}") 19)

        ; multiple arguments (6)
        (test (run "{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}") 1099)
        (test (run "{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}") 11053)
        (test (run "{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}") 0)
        (test (run "{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}") 64)
        (test (run "{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}") 'function)
        (test (run "{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}}") 42)

        ; exceptions (4)
        (test (run "{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}") 4242)
        (test (run "{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}") 33)
        (test (run "{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}") 0)
        (test (run "{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}") 4242)