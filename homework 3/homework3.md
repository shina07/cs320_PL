# Homework #3

Implement ***PWAE*** (Postfixed ***WAE***). Begin with [the "WAE" implementation](http://plrg.kaist.ac.kr/home/lectures/cs32012/wae) to serve as the basis of your work. As a first step, replace all of the ***WAE***s in the file with ***PWAE***.

- ##### Multiple Values
	Before implementing ***PWAE***, first extend ***WAE*** so that instead of plain numeric values we actually deal with multiple values. As a representation for this, we replace the ***number*** in the output of ***eval*** (and ***run***) with ***(listof number)***.

- Fixing the Arithmetic Operators
	Next, we need to fix the arithmetic operators. This is a bit tricky, since each of them receives two inputs that are both lists of numbers, and they should apply the operator on each pair from these two inputs, and collect a list of all of the results. So to make it easy, here is a skeleton of a utility function that will do this work. It is near-complete, and you have a tiny hole to fill:

    ;bin-op : (number number -> number) (listof number) (listof number) -> (listof number))
    ;; applies a binary numeric function on all combinations of numbers from
    ;; the two input lists, and return the list of all of the results
    (define (bin-op op ls rs)
      (define (helper l rs)
        ;; f : number -> number
        ...
        (map f rs))
      (if (null? ls)
        null
        (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

	Here are some tests that should work once you’re done with this part:

        (test (run "{+ {2 1} {3 4}}") '(5 6 4 5))
        (test (run "{+ {- {+ 1 3} 2} {10 -10}}") '(12 -8))

- ##### Fixing the ***with***
	The next that requires fixing is the one used in the evaluation of ***with***. The problem there is that we’re still wrapping the numeric result of ***eval*** in a ***num*** so we can use it with ***subst***, and num expects a single number.
	One way to resolve this would be to add a new variant called nums to our AST definition. But this would require reworking new cases for it in a few places. So instead, we will choose an easier solution: just change the existing num so instead of holding a single ***number*** it will hold a ***(listof number)***. Once you do that, you will have three easy fixes to do. First, the code that parses numbers should put a list with the number in the num. Next, there are two small fixes left in the ***eval*** function, and everything will work fine with it.
	Don’t forget to add tests that demonstrate that this works: that using ***with*** to bind a name to a multi-valued expression works as expected. (You need to do this test even though you should already have complete coverage at this point.) Here are some tests that should work once you’re done with this part:
    
        (test (run "{+ 3 7}") '(10))
        (test (run "{- 10 {3 5}}") '(7 5))
        (test (run "{with {x {+ 5 5}} {+ x x}}") '(20))

- ##### Extending the Syntax
	Now, extend ***WAE*** to ***PWAE*** in a simple way: a single new pooh form is added to the language, which can look like this: ***{pooh {pooh 1 2 -} 5 {pooh 3 4 -} +}***. In the ***pooh*** form there are two or more ***PWAE*** expressions, and an arithmetic operator name. Note that operator names in ***WAE*** and in ***PWAE*** are not expressions — so using them inside a ***pooh*** form should not be taken as expressions either.
	Extend the formal grammar for the language first, and the data type definition next. Extend the main BNF with the new ***pooh*** form. Note that mixing postfix forms with the usual prefix syntax is possible, for example:
    
       {+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}
       {pooh {+ {pooh 1 2 -} 5} {- 3 4} +}
       {pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}
       {+ {+ {- 1 2} 5} {- 3 4}}
       {with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}
   
	are all valid syntaxes, and they will evaluate to the same result as above.
	You may want to use library functions for lists such as ***map***, ***foldl***, and ***drop-right***. Here are some tests:
    
    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))

Here are some more tests:

    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{pooh 1 2 -}") '(-1))
    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
    (test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
    (test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
    (test (run "{pooh 1 2 3 4 5 +}") '(15))
    (test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))
    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{pooh 1 2 3 4 +}") '(10))
    (test (run "{pooh {3 4} {-4 0} 5 +}") '(4 8 5 9))
    (test (run "{pooh 1 2 3 4 -}") '(-8))
    (test (run "{pooh {4 1} 1 {5 6 7} -}") '(-2 -3 -4 -5 -6 -7))
    (test (run "{+ {pooh 1 {4 9} -3 {2 0 1} +} {- {pooh {3 4} {2} -} 4}}") '(1 2 -1 0 0 1 6 7 4 5 5 6))
    (test (run "{pooh 1 {pooh 1 2 -} 3 +}") '(3))
    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{pooh {2 1} {3 4} +}") '(5 6 4 5))
    (test (run "{with {x {1 2}} {pooh x {+ {1 2} 1} -}}") '(-1 -2 0 -1))
    (test (run "{with {x {1 2}} {pooh x {pooh {1 2} 1 +} -}}") '(-1 -2 0 -1))
    (test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
    (test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
    (test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
    (test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{with {x {with {y {1 -2}} {pooh 1 y 2 -}}} {+ x x}}") '(-4 -1 -1 2))
    (test (run "{pooh {0 1} {2 3} {4 5} 6 +}") '(12 13 13 14 13 14 14 15))
    (test (run "{with {x {pooh 8 7 -}} {with {x 10} {+ x 3}}}") '(13))
    (test (run "{pooh {pooh 1 2 {2 3} {1 2} -} {2 1 3 2} {1 2} +}") '(-1 0 -2 -1 0 1 -1 0 -2 -1 -3 -2 -1 0 -2 -1 -2 -1 -3 -2 -1 0 -2 -1 -3 -2 -4 -3 -2 -1 -3 -2))
    (test (run "{with {x {pooh {1 2} {2 3} 1 +}} {pooh x 1 2 +}}") '(7 8 8 9))
    (test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
    (test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
    (test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
    (test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))