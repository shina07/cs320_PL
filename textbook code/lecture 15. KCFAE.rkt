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


;; KCFAE abstract syntax trees
(define-type KCFAE
  [num     (n number?)]
  [add     (lhs KCFAE?)
           (rhs KCFAE?)]
  [sub     (lhs KCFAE?)
           (rhs KCFAE?)]
  [id      (name symbol?)]
  [fun     (param symbol?)
           (body KCFAE?)]
  [app     (fun-expr KCFAE?)
           (arg-expr KCFAE?)]
  [if0     (test-expr KCFAE?)
           (then-expr KCFAE?)
           (else-expr KCFAE?)]
  [withcc  (name symbol?)
           (body KCFAE?)]
)

(define-type KCFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?)
             (body KCFAE?)
             (ds DefrdSub?)]
  [contV     (proc procedure?)]
)

(define-type DefrdSub
  [mtSub]
  [aSub    (name symbol?)
           (value KCFAE-Value?)
           (ds DefrdSub?)]
)

;; parse-sexpr : S-expr -> KCFAE
;; to convert s-expressions into KCFAEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(pair? sexp)
     (case (car sexp)
       [(+) (add (parse (second sexp)) (parse (third sexp)))]
       [(-) (sub (parse (second sexp)) (parse (third sexp)))]
       [(fun) (fun (first (second sexp)) (parse (third sexp)))]
       [(if0) (if0 (parse (second sexp))
                   (parse (third sexp))
                   (parse (fourth sexp)))]
       [(withcc) (withcc (second sexp) (parse (third sexp)))]
       [else (app (parse (first sexp)) (parse (second sexp)))])]))

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
    [mtSub     () (error 'lookup "free variable: ~a" name)]
    [aSub      (x val rest)
               (if (symbol=? x name)
                   val
                   (lookup name rest))]
  )
)

;; interp: KCFAE DefrdSub (KCFAE-Value -> alpha) -> alpha
;; evaluates KCFAE expressions by reducing them to numbers
(define (interp kcfae ds k)
  (type-case KCFAE kcfae
    [num (n)      (k (numV n))]
    [add (l r)    (interp l ds
                          (lambda (v1)
                            (interp r ds
                                    (lambda (v2)
                                      (k (num+ v1 v2))))))]
    [sub (l r)    (interp l ds
                          (lambda (v1)
                            (interp r ds
                                    (lambda (v2)
                                      (k (num- v1 v2))))))]
    [id (s)       (k (lookup s ds))]
    [fun (x b)    (k (closureV x b ds))]
    [app (f a)    (interp f ds
                          (lambda (fun-val)
                            (interp a ds
                                    (lambda (arg-val)
                                      (type-case KCFAE-Value fun-val
                                        [closureV (param body ds2)
                                                  (interp body
                                                          (aSub param
                                                                arg-val
                                                                ds2)
                                                          k)]
                                        [contV    (k)
                                                  (k arg-val)]
                                        [else     (error 'interp "not a function")])))))]
    [if0 (test-expr then-expr else-expr)
                  (interp test-expr ds
                          (lambda (v)
                            (if (numzero? v)
                                (interp then-expr ds k)
                                (interp else-expr ds k))))]
    [withcc (id body)
                  (interp body
                          (aSub id
                                (contV k)
                                ds)
                          k)]
  )
)

(define (numzero? n)
  (zero? (numV-n n))
)


; run : string -> listof number
;; evaluate a KCFAE program contained in a string
;(define (run str)
;  (interp (parse str) (mtSub) (lambda (x) x)))

;; interp-expr : KCFAE -> number-or-'function
(define (interp-expr kcfae)
  (type-case KCFAE-Value (interp kcfae (mtSub) (lambda (x) x))
    [numV     (n)             n]
    [closureV (param body ds) 'function]
    [contV    (k)             'function]
  )
)

;; run : string -> num-or-'function
(define (run str)
  (interp-expr (parse (string->sexpr str)))
)

;; Test Implementation
   ;; Parse
(test (parse 3) (num 3))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{fun {x} x}) (fun 'x (id 'x)))
(test (parse '{1 2}) (app (num 1) (num 2)))
(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{withcc x 2}) (withcc 'x (num 2)))

   ;; Lookup
(test/exn (lookup 'x (mtSub)) "free variable")
(test (lookup 'x (aSub 'x (numV 9) (mtSub))) (numV 9))
(test (lookup 'x (aSub 'y (numV 10) (aSub 'x (numV 9) (mtSub)))) (numV 9))


   ;; Run
(test (run "1") 1)
(test (run "{+ 1 2}") 3)
(test (run "{withcc k {+ 1 {k 2}}}") 2)
(test (run "{ withcc esc {{fun {x y} x} 1 {esc 3}} }") 1)
(test (run "{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}") 4)