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


;; RCFAE abstract syntax trees
(define-type RCFAE
  [num     (n number?)]
  [add     (lhs RCFAE?) (rhs RCFAE?)]
  [sub     (lhs RCFAE?) (rhs RCFAE?)]
  [id      (name symbol?)]
  [fun     (param symbol?)(body RCFAE?)]
  [app     (ftn RCFAE?) (arg RCFAE?)]
  [if0     (test-expr RCFAE?)(then-expr RCFAE?)(else-expr RCFAE?)]
  [rec     (name symbol?)(named-expr RCFAE?)(body RCFAE?)]
)

(define-type RCFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?)(body RCFAE?)(ds DefrdSub?)]
)

(define-type DefrdSub
  [mtSub]
  [aSub    (name symbol?)(value RCFAE-Value?)(ds DefrdSub?)]
  [aRecSub (name symbol?)(value (box/c RCFAE-Value?))(ds DefrdSub?)]
)

;; parse-sexpr : sexpr -> RCFAE
;; to convert s-expressions into RCFAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r)             (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (app (fun x (parse-sexpr b)) (parse-sexpr i))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list x) b)    (fun x (parse-sexpr b))]

    [(list 'if0 test then else)(if0 (parse-sexpr test)(parse-sexpr then)(parse-sexpr else))]
    [(list 'rec (list s e) b)  (rec s (parse-sexpr e)(parse-sexpr b))]
    
    [(list f a)                (app (parse-sexpr f) (parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a RCFAE expression to a RCFAE AST
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
    [mtSub     () (error 'lookup "free identifier: ~a" name)]
    [aSub      (x val rest)
               (if (symbol=? x name)
                   val
                   (lookup name rest))]
    [aRecSub   (sub-name val-box rest-ds)
               (if (symbol=? sub-name name)
                   (unbox val-box)
                   (lookup name rest-ds))]
  )
)

;; interp: RCFAE DefrdSub -> RCFAE-Value
;; evaluates RCFAE expressions by reducing them to numbers
(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n)      (numV n)]
    [add (l r)    (num+ (interp l ds)(interp r ds))]
    [sub (l r)    (num- (interp l ds)(interp r ds))]
    [id (s)       (lookup s ds)]
    [fun (x b)    (closureV x b ds)]
    [app (f a)    (local [(define ftn (interp f ds))]
                    (interp (closureV-body ftn)
                            (aSub (closureV-param ftn)
                                  (interp a ds)
                                  (closureV-ds ftn))))]
    [if0 (test-expr then-expr else-expr)
         (if (numzero? (interp test-expr ds))
             (interp then-expr ds)
             (interp else-expr ds))]
    [rec (bound-id named-expr body-expr)
      (local [(define value-holder (box (numV 42)))
              (define new-ds (aRecSub bound-id
                                      value-holder
                                      ds))]
        (begin
          (set-box! value-holder (interp named-expr new-ds))
          (interp body-expr new-ds)))]
  )
)

(define (numzero? n)
  (zero? (numV-n n))
)


; run : string -> listof number
;; evaluate a RCFAE program contained in a string
(define (run str)
  (interp (parse str) (mtSub)))

(run "1")
(run "{+ 1 2}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")

(run "{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}}
           {count 8}}")