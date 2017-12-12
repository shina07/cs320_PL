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

;; WAE abstract syntax trees
(define-type WAE
  [num  (num number?)]
  [add  (left WAE?) (right WAE?)]
  [sub  (left WAE?) (right WAE?)]
  [with (name symbol?) (init WAE?) (body WAE?)]
  [id   (name symbol?)])

; parse-sexpr : sexpr -> WAE
;; to convert s-expressions into WAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a WAE expression to a WAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst wae x val)
  (type-case WAE wae
    [num (n)       wae]
    [add (l r)     (add (subst l x val) (subst r x val))]
    [sub (l r)     (sub (subst l x val) (subst r x val))]
    [id (s)        (if (symbol=? s x)
                       (num val)
                       wae)]
    [with (y i b)  (with y
                         (subst i x val)
                         (if (symbol=? y x)
                                       b
                                       (subst b x val)))]
  )
)

;; evaluates WAE expressions by reducing them to numbers
(define (interp wae)
  (type-case WAE wae
    [num (n)      n]
    [add (l r)    (+ (interp l) (interp r))]
    [sub (l r)    (- (interp l) (interp r))]
    [with (y i b) (interp (subst b y (interp i)))]
    [id (s)    (error 'interp "free identifier: ~s" s)]
  )
)

; run : string -> listof number
;; evaluate a WAE program contained in a string
(define (run str)
  (interp (parse str)))

;; tests
(test (run "5") 5)
(test (run "{+ 5 5}") 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") 20)
(test (run "{with {x 5} {+ x x}}") 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") 10)
(test (run "{with {x 5} {with {y x} y}}") 5)
(test (run "{with {x 5} {with {x x} x}}") 5)
(test/exn (run "{with {x 1} y}") "free identifier")

;; additional tests for complete coverage
(test (run "{with {x 2} {- {+ x x} x}}") 2)
(test/exn (run "{with x = 2 {+ x 3}}") "bad syntax")
(test/exn (run "{bleh}") "bad syntax")

;; private testing
(string->sexpr "{with {x {+ 5 5}} {+ x x}}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(interp (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))
(parse "{with {x 5} {+ x {with {y 3} x}}}")

;; Subst Testing in lecture slide
(subst (with 'y (num 17) (id 'x)) 'x 10)
(subst (with 'y (id 'x) (id 'y)) 'x 10)
(subst (with 'x (id 'y) (id 'x)) 'x 10)