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


;; FWAE abstract syntax trees
(define-type FWAE
  [num  (n number?)]
  [add  (lhs FWAE?) (rhs FWAE?)]
  [sub  (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id   (name symbol?)]
  [fun  (param symbol?)(body FWAE?)]
  [app  (ftn FWAE?) (arg FWAE?)]
)

;; parse-sexpr : sexpr -> FWAE
;; to convert s-expressions into FWAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r)             (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?)               (id sexp)]
    [(list 'fun x b)           (fun x (parse-sexpr b))]
    [(list f a)                (app (parse-sexpr f) (parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a FWAE expression to a FWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))
  )
)

(define num+ (num-op +))
(define num- (num-op -))

;; subst : FWAE symbol FWAE -> FWAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst fwae x val)
  (type-case FWAE fwae
    [num (n)       fwae]
    [add (l r)     (add (subst l x val) (subst r x val))]
    [sub (l r)     (sub (subst l x val) (subst r x val))]
    [id (s)        (if (equal? s x)
                       (num val)
                       fwae)]
    [with (y i b)  (with y
                         (subst i x val)
                         (if (symbol=? y x)
                                       b
                                       (subst b x val)))]
    [fun (y b)     (if (equal? x y)
                       fwae
                       (fun id (subst b x val)))]    
    [app (f a)     (app (subst f x val)(subst a x val))]
  )
)

;; interp: FWAE -> FWAE
;; evaluates FWAE expressions by reducing them to numbers
(define (interp fwae)
  (type-case FWAE fwae
    [num (n)      fwae]
    [add (l r)    (num+ (interp l) (interp r))]
    [sub (l r)    (num- (interp l) (interp r))]
    [with (y i b) (interp (subst b y (interp i)))]
    [id (s)    (error 'interp "free identifier: ~s" s)]
    [fun (x b)    (fwae)]
    [app (f a)    (local [(define ftn (interp f))]
                    (interp (subst (fun-body ftn)
                                   (fun-param  ftn)
                                   (interp a))))]
  )
)

; run : string -> listof number
;; evaluate a FWAE program contained in a string
(define (run str)
  (interp (parse str)))

(run "1")
(run "{+ 1 2}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")