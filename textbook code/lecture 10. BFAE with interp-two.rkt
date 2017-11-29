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


;; BFAE abstract syntax trees
(define-type BFAE
  [num     (n number?)]
  [add     (lhs BFAE?) (rhs BFAE?)]
  [sub     (lhs BFAE?) (rhs BFAE?)]
  [id      (name symbol?)]
  [fun     (param symbol?)(body BFAE?)]
  [app     (ftn BFAE?) (arg BFAE?)]
  [newbox  (val BFAE?)]
  [setbox  (box BFAE?)(val BFAE?)]
  [openbox (box BFAE?)]
  [seqn    (expr1 BFAE?)(expr2 BFAE?)]
)

(define-type BFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?)(body BFAE?)(ds DefrdSub?)]
  [boxV      (address integer?)]
)

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value BFAE-Value?)(ds DefrdSub?)]
)

(define-type Store
  [mtSto]
  [aSto (address integer?)(value BFAE-Value?)(rest Store?)]
)

(define-type Value*Store
  [v*s (value BFAE-Value?)(store Store?)]
)

;; parse-sexpr : sexpr -> BFAE
;; to convert s-expressions into BFAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r)             (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (app (fun x (parse-sexpr b)) (parse-sexpr i))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list x) b)    (fun x (parse-sexpr b))]
   
    [(list 'newbox v)          (newbox (parse-sexpr v))]
    [(list 'setbox b v)        (setbox (parse-sexpr b) (parse-sexpr v))]
    [(list 'openbox b)         (openbox (parse-sexpr b))]
    [(list 'seqn a b)          (seqn (parse-sexpr a)(parse-sexpr b))]

    [(list f a)                (app (parse-sexpr f) (parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parses a string containing a BFAE expression to a BFAE AST
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
    [mtSub () (error 'lookup "free identifier: ~a" name)]
    [aSub (x val rest)
          (if (symbol=? x name)
              val
              (lookup name rest))]
  )
)

(define (store-lookup addr st)
  (type-case Store st
    [mtSto () (error 'store-lookup "no such address")]
    [aSto (i val rest)
          (if (equal? i addr)
              val
              (store-lookup addr rest))]
  )
)

;; interp: BFAE DefrdSub Store -> Value*Store
;; evaluates BFAE expressions by reducing them to numbers
(define (interp bfae ds st)
  (type-case BFAE bfae
    [num (n)      (v*s (numV n) st)]
    [add (l r)    (interp-two l r ds st
                              (lambda (v1 v2 st1) (v*s (num+ v1 v2) st)))]
    [sub (l r)    (interp-two l r ds st
                              (lambda (v1 v2 st1) (v*s (num- v1 v2) st)))]
    [id (s)       (v*s (lookup s ds) st)]
    [fun (x b)    (v*s (closureV x b ds) st)]
    [app (f a)    (interp-two f a ds st
                              (lambda (f-val a-val st1)
                                (interp (closureV-body f-val)
                                        (aSub (closureV-param f-val)
                                              a-val
                                              (closureV-ds f-val))
                                        st1)))]
    [newbox (v)   (type-case Value*Store (interp v ds st)
                    [v*s (v1 st1)
                         (local [(define a (malloc st1))]
                           (v*s (boxV a)
                                (aSto a v1 st1)))])]
    [setbox (b v) (interp-two b v ds st
                              (lambda (b-val v-val st1)
                                (v*s v-val
                                     (aSto (boxV-address b-val)
                                           v-val
                                           st1))))]
    [openbox (b)  (type-case Value*Store (interp b ds st)
                    [v*s (b st1)
                         (v*s (store-lookup (boxV-address b)
                                            st1)
                              st1)])]
    [seqn (a b)   (interp-two a b ds st
                              (lambda (v1 v2 st1) (v*s v2 st1)))]
  )
)

;; interp-two : BFAE BFAE DefrdSub Store (Value Value Store -> Value*Store) -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3) (handle val1 val2 st3)]
         ]
    ]
  )
)

;; malloc : Store -> integer
(define (malloc st)
  (+ 1 (max-address st))
)

;; max-address : Store -> integer
(define (max-address st)
  (type-case Store st
    [mtSto ()       0]
    [aSto  (n v st) (max n (max-address st))]
  )
)

; run : string -> listof number
;; evaluate a BFAE program contained in a string
(define (run str)
  (interp (parse str) (mtSub) (mtSto)))

(run "1")
(run "{+ 1 2}")
(parse "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")
(test (run "{with {b {newbox 0}}{seqn {setbox b 10}{openbox b}}}")
      (v*s
       (numV 10)
       (aSto 1 (numV 10) (aSto 1 (numV 0) (mtSto)))))
(test (run "{with {q {newbox 10}}{setbox {seqn {setbox q 12} q}{openbox q}}}")
      (v*s
       (numV 12)
       (aSto 1 (numV 12) (aSto 1 (numV 12) (aSto 1 (numV 10) (mtSto))))))
