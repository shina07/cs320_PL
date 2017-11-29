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

;; Function Definition
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)]
)

;; F1WAE abstract syntax trees
(define-type F1WAE
  [num  (num number?)]
  [add  (left F1WAE?) (right F1WAE?)]
  [sub  (left F1WAE?) (right F1WAE?)]
  [with (name symbol?) (init F1WAE?) (body F1WAE?)]
  [id   (name symbol?)]
  [app  (ftn symbol?) (arg F1WAE?)]
)

;; DefrdSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefrdSub?)]
)

;; parse-fd : sexp -> FunDef
(define (parse-sexpr-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse-sexpr b))]
  )
)

;; parse-list-fd : sexp -> list-of-FunDef
(define (parse-sexpr-list-fd sexp)
  (cond
    [(empty? sexp) empty]
    [else (cons (parse-sexpr-fd (first sexp)) (parse-sexpr-list-fd (rest sexp)))]
  )
)

(define (parse-list-fd str)
  (parse-sexpr-list-fd (string->sexpr str))
)
;; where do we need this?
(define (parse-fd str)
  (parse-sexpr-fd (string->sexpr str)))

;; parse-sexpr : sexpr -> F1WAE
;; to convert s-expressions into F1WAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse-sexpr a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a F1WAE expression to a F1WAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; lookup-fundef : symbol list-of-FunDef -> FunDef
(define (lookup-fundef name fundefs)
  (cond
    [(empty? fundefs)
     (error 'lookup-fundef "unknown function")]
    [else (if (symbol=? name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef name (rest fundefs))
          )]
  )
)

;; lookup : symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error `lookup "free identifier: ~a" name)]
    [aSub (x val rest) (if (symbol=? x name)
                           val
                           (lookup name rest))]
  )
)

;; inerp: F1WAE list-of-FunDef DefrdSub -> number
;; evaluates F1WAE expressions by reducing them to numbers
(define (interp f1wae fundefs ds)
  (type-case F1WAE f1wae
    [num (n)      n]
    [add (l r)    (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r)    (- (interp l fundefs ds) (interp r fundefs ds))]
    [with (y i b) (interp b fundefs (aSub y (interp i fundefs ds) ds))]
    [id (s)       (lookup s ds)]
    [app (f a)
         (local
           [(define a-fundef (lookup-fundef f fundefs))]
           (interp (fundef-body a-fundef)
                   fundefs
                   (aSub (fundef-arg-name a-fundef)
                         (interp a fundefs ds)
                         (mtSub))))]
  )
)

; run : string -> listof number
;; evaluate a F1WAE program contained in a string
(define (run str1 str2)
  (interp (parse str1) (parse-list-fd str2) (mtSub)))

;; tests
(test (run "5" "{}") 5)
(test (run "{+ 5 5}" "{}") 10)
(test (run "{with {x {+ 5 5}} {+ x x}}" "{}") 20)
(test (run "{with {x 5} {+ x x}}" "{}") 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}" "{}") 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}" "{}") 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}" "{}") 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}" "{}") 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}" "{}") 10)
(test (run "{with {x 5} {with {y x} y}}" "{}") 5)
(test (run "{with {x 5} {with {x x} x}}" "{}") 5)
(test/exn (run "{with {x 1} y}" "{}") "free identifier")

;; additional tests for complete coverage
(test (run "{with {x 2} {- {+ x x} x}}" "{}") 2)
(test/exn (run "{with x = 2 {+ x 3}}" "{}") "bad syntax")
(test/exn (run "{bleh}" "{}") "bad syntax")

;; private testing
(string->sexpr "{with {x {+ 5 5}} {+ x x}}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(interp (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))) empty (mtSub))

(parse-fd "{deffun {identity x} x}")
(fundef 'identity 'x (id 'x))
(app 'identity (num 8))

(interp (add (num 1) (num 1)) empty (mtSub))
(interp (add (num 1) (num 1)) (list (fundef 'f 'x (add (id 'x) (num 3)))) (mtSub))
(interp (app 'f (num 1)) (list (fundef 'f 'x (add (id 'x) (num 3)))) (mtSub))
(interp (app 'f (num 10))
        (list (fundef 'f 'x (sub (num 20)
                                 (app 'twice (id 'x))))
              (fundef 'twice 'y (add (id 'y) (id 'y))))
        (mtSub))


(interp (num 5) empty (mtSub))
(string->sexpr "{+ 5 5}")
(string->sexpr "{}")
(parse-list-fd "{}")
(parse-list-fd "{{deffun {f x} {+ x 3}}}")

(string->sexpr "{with {x {+ 5 5}} {+ x {f 10}}}")
(string->sexpr "{{deffun {f x} {- 20 {twice x}}}{deffun {twice y} {+ y y}}}")
(parse "{with {x {+ 5 5}} {+ x {f 10}}}")
(parse-list-fd "{{deffun {f x} {- 20 {twice x}}}{deffun {twice y} {+ y y}}}")
(run "{with {x {+ 5 5}} {+ x {f 10}}}" "{{deffun {f x} {- 20 {twice x}}}{deffun {twice y} {+ y y}}}")


;; lecture 6 - DefrdSub QnA test
;(run "{twice twice}" "{{deffun {twice x} {+ x x}}}")
;(run "{with {x 5} {x 7}}" "{{deffun {x y} y}}")
;(run "{+ 3}" "{{deffun {+ x} x}}")
;(run "{f 3}" "{{deffun {f f} f}}")
;(run "{f f}" "{{deffun {f f} f}}")

; lecture 6 - Function Calls
;(run "{with {y 2} {f 10}}" "{{deffun {f x} {+ 1 x}}}")