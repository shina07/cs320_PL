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


;; FAE abstract syntax trees
(define-type FAE
  [num  (n number?)]
  [add  (lhs FAE?) (rhs FAE?)]
  [sub  (lhs FAE?) (rhs FAE?)]
  [id   (name symbol?)]
  [fun  (param symbol?)(body FAE?)]
  [app  (ftn FAE?) (arg FAE?)]
)

(define-type FAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?)(body FAE?)(ds DefrdSub?)]
)

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value FAE-Value?)(ds DefrdSub?)]
)

;; parse-sexpr : sexpr -> FAE
;; to convert s-expressions into FAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r)             (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (app (fun x (parse-sexpr b)) (parse-sexpr i))]
    [(? symbol?)               (id sexp)]
    [(list 'fun x b)           (fun x (parse-sexpr b))]
    [(list f a)                (app (parse-sexpr f) (parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a FAE expression to a FAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))
  )
)

(define num+ (num-op +))
(define num- (num-op -))

;; lookup : symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (x val rest)
          (if (symbol=? x name)
              val
              (lookup name rest))]
  )
)

;; inerp: FAE DefrdSub -> FAE-Value
;; evaluates FAE expressions by reducing them to numbers
(define (interp fae ds)
  (type-case FAE fae
    [num (n)      (numV n)]
    [add (l r)    (num+ (interp l ds) (interp r ds))]
    [sub (l r)    (num- (interp l ds) (interp r ds))]
    [id (s)       (lookup s ds)]
    [fun (x b)    (closureV x b ds)]
    [app (f a)    (local [(define f-val (interp f ds))
                          (define a-val (interp a ds))]
                    (interp (closureV-body f-val)
                            (aSub (closureV-param f-val)
                                  a-val
                                  (closureV-ds f-val))))]
  )
)

; run : string -> listof number
;; evaluate a FAE program contained in a string
(define (run str)
  (interp (parse str) (mtSub)))

(run "1")
(run "{+ 1 2}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")