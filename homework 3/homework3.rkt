#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

;; PWAE abstract syntax trees
(define-type PWAE
  [num  (num (listof number?))]
  [add  (left PWAE?) (right PWAE?)]
  [sub  (left PWAE?) (right PWAE?)]
  [with (name symbol?) (init PWAE?) (body PWAE?)]
  [id   (name symbol?)])

; parse-sexpr : sexpr -> PWAE                                   [Contract]

;; to convert s-expressions into PWAEs                          [Purpose]

(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num (list sexp))]
    [(? (listof number?)) (num sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?) (id sexp)]
    [(list 'pooh l r ... '+) (foldl (lambda (n acc) (add acc (parse-sexpr n))) (add (parse-sexpr l) (parse-sexpr (first r))) (rest r))]
    [(list 'pooh l r ... '-) (foldl (lambda (n acc) (sub acc (parse-sexpr n))) (sub (parse-sexpr l) (parse-sexpr (first r))) (rest r))]
    [else (error 'parse "bad syntax: ~a" sexp)]))


;; parses a string containing a PWAE expression to a PWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;bin-op : (listof number) (listof number) -> (listof number)    [Contract]   

;; applies a binary numeric function on all combinations of     [Purpose]
;; numbers from the two input lists, and return the list of all
;; of the results

(define (bin-op op ls rs)
  (define (helper l rs)
    ;; f : number -> number
    (define (f r)
      (cond
        [(eq? op +) (+ l r)]
        [(eq? op -) (- l r)]
        [else (error "Invalid Aritmatic Operator")]
      )
    )
    (map f rs)
  )
  (if (null? ls)
    null
    (append (helper (first ls) rs) (bin-op op (rest ls) rs))))


; subst : PWAE symbol symbol -> PWAE                            [Contract]

;; substitutes the second argument with the third argument in   [Purpose]
;; the first argument, as per the rules of substitution; the
;; resulting expression contains no free instances of the second
;; argument
(define (subst expr from to)
  (type-case PWAE expr
    [num (n)   expr]
    [add (l r) (add (subst l from to) (subst r from to))]
    [sub (l r) (sub (subst l from to) (subst r from to))]
    [id (name) (if (symbol=? name from) (num to) expr)]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr from to)
                (if (symbol=? bound-id from)
                    bound-body
                    (subst bound-body from to)))]))


; eval : PWAE -> (listof number)                               [Contract]

;; evaluates PWAE expressions by reducing them to numbers      [Purpose]
(define (eval expr)
  (type-case PWAE expr
    [num (n) n]
    [add (l r) (bin-op + (eval l) (eval r))]
    [sub (l r) (bin-op - (eval l) (eval r))]
    [with (bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval named-expr)))]
    [id (name) (error 'eval "free identifier: ~s" name)])) 


; run : string -> (listof number)                              [Contract]

;; evaluate a PWAE program contained in a string               [Purpose]
(define (run str)
  (eval (parse str)))


;; Revised Test sets

; Fixing the Arithmetic Operators (Each steps represents the order of implementation)
(test (string->sexpr "{+ {2 1} {3 4}}") '(+ (2 1) (3 4)))
(test (parse-sexpr '(+ (2 1) (3 4))) (add (num '(2 1)) (num '(3 4))))
(test/exn (parse-sexpr '(+ 1 2 3 4)) "parse: bad syntax: (+ 1 2 3 4)")

(test (bin-op + '(2 1) '(3 4)) '(5 6 4 5))
(test (bin-op + '() '(1 2 3 4)) '())

(test (eval (num '(2 1))) '(2 1))
(test (eval (add (num '(2 1)) (num '(3 4)))) '(5 6 4 5))

(test (run "{+ {2 1} {3 4}}") '(5 6 4 5))
(test (run "{+ {- {+ 1 3} 2} {10 -10}}") '(12 -8))

; Fixing the with (Nothing changed)
(test (run "{+ 3 7}") '(10))
(test (run "{- 10 {3 5}}") '(7 5))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))

; Extending the Syntax
(test (string->sexpr "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(+ (+ (pooh 1 2 -) 5) (pooh 3 4 -)))
(test (string->sexpr "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(pooh (+ (pooh 1 2 -) 5) (- 3 4) +))
(test (string->sexpr "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(pooh (pooh (pooh 1 2 -) 5 +) (pooh 3 4 -) +))
(test (string->sexpr "{+ {+ {- 1 2} 5} {- 3 4}}") '(+ (+ (- 1 2) 5) (- 3 4)))
(test (string->sexpr "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(with (x (pooh 3 4 -)) (pooh (+ (pooh 1 2 -) 5) x +)))

(test (parse-sexpr '(+ (+ (pooh 1 2 -) 5) (pooh 3 4 -))) (add (add (sub (num '(1)) (num '(2))) (num '(5))) (sub (num '(3)) (num '(4)))))
(test (parse-sexpr '(pooh (- 3 4) (- 4 5) (- 5 6) +)) (add (add (sub (num '(3)) (num '(4))) (sub (num '(4)) (num '(5)))) (sub (num '(5)) (num '(6)))))
(test (parse-sexpr '(pooh (+ (pooh 1 2 -) 5) (- 3 4) +)) (add (add (sub (num '(1)) (num '(2))) (num '(5))) (sub (num '(3)) (num '(4)))))
(test (parse-sexpr '(pooh (pooh (pooh 1 2 -) 5 +) (pooh 3 4 -) +)) (add (add (sub (num '(1)) (num '(2))) (num '(5))) (sub (num '(3)) (num '(4)))))
(test (parse-sexpr '(+ (+ (- 1 2) 5) (- 3 4))) (add (add (sub (num '(1)) (num '(2))) (num '(5))) (sub (num '(3)) (num '(4)))))
(test (parse-sexpr '(with (x (pooh 3 4 -)) (pooh (+ (pooh 1 2 -) 5) x +))) (with 'x (sub (num '(3)) (num '(4))) (add (add (sub (num '(1)) (num '(2))) (num '(5))) (id 'x))))

(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))

; Some Personal Tests
(test (run "{pooh {pooh 1 2 3 4 5 +} {pooh 1 2 3 4 5 -} {pooh 1 2 3 4 5 +} +}") '(17))
(test (run "{pooh {pooh 1 2 3 4 5 +} {pooh 1 2 3 4 5 -} {pooh 1 2 3 4 5 +} -}") '(13))


; More Test Cases (Which is given)
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