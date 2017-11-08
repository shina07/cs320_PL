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
    (num (op (num-n x) (num-n y)))
  )
)

(define num+ (num-op +))
(define num- (num-op -))

;; subst : FWAE symbol FWAE -> FWAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst fae x val)
  (type-case FAE fae
    [num (n)       fae]
    [add (l r)     (add (subst l x val) (subst r x val))]
    [sub (l r)     (sub (subst l x val) (subst r x val))]
    [id (s)        (if (equal? s x)
                       (num val)
                       fae)]
    [fun (y b)     (if (equal? x y)
                       fae
                       (fun id (subst b x val)))]    
    [app (f a)     (app (subst f x val)(subst a x val))]
  )
)

;; interp: FAE -> FAE
;; evaluates FAE expressions by reducing them to numbers
(define (interp fae)
  (type-case FAE fae
    [num (n)      fae]
    [add (l r)    (num+ (interp l) (interp r))]
    [sub (l r)    (num- (interp l) (interp r))]
    [id (s)    (error 'interp "free identifier: ~s" s)]
    [fun (x b)    (fae)]
    [app (f a)    (local [(define ftn (interp f))]
                    (interp (subst (fun-body ftn)
                                   (fun-param  ftn)
                                   (interp a))))]
  )
)

; run : string -> listof number
;; evaluate a FAE program contained in a string
(define (run str)
  (interp (parse str)))

(run "1")
(run "{+ 1 2}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")