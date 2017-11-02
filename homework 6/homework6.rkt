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
  [num    (n number?)]
  [add    (lhs FWAE?) (rhs FWAE?)]
  [sub    (lhs FWAE?) (rhs FWAE?)]
  [with   (name symbol?)(named-expr FWAE?) (body FWAE?)]
  [id     (name symbol?)]
  [fun    (params (listof symbol?))(body FWAE?)]
  [app    (ftn FWAE?) (args (listof FWAE?))]
  [rec (records (listof Record?))]
  [getrec (rec-expr FWAE?)(id symbol?)]
)

(define-type FWAE-Value
  [numV      (n number?)]
  [closureV  (params (listof symbol?))(body FWAE?)(ds DefrdSub?)]
  [recV      (records (listof Record-Value?))]
)

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(value FWAE-Value?)(ds DefrdSub?)]
)

(define-type Record
  [record (id symbol?)(expr FWAE?)]
)

(define-type Record-Value
  [recval (id symbol?)(value FWAE-Value?)]
)

;; Built-in Functions (reserved symbols)
(define built-in '(+ - with fun))

;; parse-sexpr : sexpr -> FWAE
;; to convert s-expressions into FWAEs
(define (parse-sexpr sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r)             (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list x ...) b)(parse-fun sexp)]

    [(list 'record r ...)         (parse-rec sexp)]
    [(list 'getField e x)        (getrec (parse-sexpr e) x)]
    
    [(list f a ...)     (app (parse-sexpr f) (map parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parse-fun : sexpr -> FWAE                    [Contract]

;; To parse "fun" constructor of FWAE. This     [Purpose]
;; function is separated to check the possible
;; errors that can only be detected in "fun"
;; and carry out parsing
(define (parse-fun sexp)
  (local [(define params (second sexp))
          (define body   (third sexp))]
    (if (check-duplicates  params)
        (error 'parse-fun "bad syntax: ~a" sexp)
        (fun params (parse-sexpr body)))
  )
)

;; parse-app : sexpr -> FWAE                     [Contract]

;; To parse "app" constructor of FWAE. This      [Purpose]
;; function is separated to check the possible
;; errors that can only be detected in "app"
;; and carry out parsing
(define (parse-app sexp)
  (local [(define ftn  (second sexp))
          (define args (third sexp))]
    (if (check-duplicates args)
        (error 'parse-app "bad syntax: ~a" sexp)
        (app ftn args)))
)

;; parse-rec : sexpr -> FWAE                     [Contract]

;; To parse "rec" constructor of FWAE. this      [Purpose]
;; function is separated to carry out multiple
;; operation for parsing all records in the
;; given expression
(define (parse-rec sexp)
  (local [(define records (rest sexp))]
    (if (check-duplicates (map (lambda (x) (first x)) records))
        (error 'parse "duplicate fields: ~a" sexp)
        (rec (map (lambda (x) (record (first x)(parse-sexpr (second x)))) records))
    )
  )
)

;; parses a string containing a FWAE expression to a FWAE AST
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

;; store : (list-of-symbol) (list-of-BFAE-Value) DefrdSub -> DefrdSub
;;                                               [Contract]

;; To store multiple arguments in ds             [Purpose]
(define (store params args ds)
  (cond
    [(empty? params)        ds]
    [(empty? args)          (error 'store "arity mismatch")]
    [(empty? (rest params)) (aSub (first params) (first args) ds)]
    [else                   (aSub (first params)
                                  (first args)
                                  (store (rest params) (rest args) ds))]
  )
)

;; record-lookup : symbol (list-of-Record) -> FWAE-Value
;;                                               [Contract]

;; To findout the name of given argument in to   [Purpose]
;; records
(define (record-lookup name records)
  (if (empty? records)
      (error 'record-lookup "no such field: ~a" name)
      (if (equal? name (recval-id (first records)))
          (recval-value (first records))
          (record-lookup name (rest records))
      )
  )
)

;; inerp: FWAE DefrdSub -> FWAE-Value
;; evaluates FWAE expressions by reducing them to numbers
(define (interp fwae ds)
  (type-case FWAE fwae
    [num (n)      (numV n)]
    [add (l r)    (num+ (interp l ds) (interp r ds))]
    [sub (l r)    (num- (interp l ds) (interp r ds))]
    [with (y i b) (local [(define f-val (interp (fun (list y) b) ds))
                          (define a-val (interp i ds))]
                    (interp (closureV-body f-val)
                            (aSub (first (closureV-params f-val))
                                  a-val
                                  (closureV-ds f-val))))]
    [id (s)       (lookup s ds)]
    [fun (x b)    (closureV x b ds)]
    [app (f a)    (local [(define f-val (interp f ds))
                          (define a-val (map (lambda (x) (interp x ds)) a))]
                    (interp (closureV-body f-val)
                            (store (closureV-params f-val)
                                   a-val
                                   (closureV-ds f-val))))]
    [rec (r)      (interp-rec r ds)]
    [getrec (e x) (local [(define e-val (interp e ds))]
                    (record-lookup x (recV-records e-val))
                  )]
  )
)

;; interp-rec : FWAE DefrdSub -> FWAE-Value       [Contract]

;; To interp rec expression, another type of      [Purpose]
;; interp function is further implemented
(define (interp-rec records ds)
  (cond
    [(empty? records)  (recV empty ds)]
    [else              (local
                         [(define rec-v (map (lambda (x)
                                               (recval (record-id x) (interp (record-expr x) ds)))
                                             records))]
                         (recV rec-v)
                       )]
  )
)

;; interp-value : FWAE-Value -> (number, 'function, or 'record)
;;                                                [Contract]

;; To show what type of value the interp returns  [Purpose]
(define (interp-expr fwae-v)
  (type-case FWAE-Value fwae-v
    [numV     (n)       n]
    [closureV (x b ds)  'function]
    [recV     (records) 'record]
  )
)


; run : string -> listof number
;; evaluate a FWAE program contained in a string
(define (run str)
  (interp-expr (interp (parse str) (mtSub))))

;; Private Tests
   ; Checking Initial Implementation
(test (interp (parse "1") (mtSub)) (numV 1))
(test (interp (parse "{+ 1 2}") (mtSub)) (numV 3))
(test (interp (parse "{with {x {+ 5 5}} {+ x x}}")(mtSub)) (numV 20))

   ; Functions with Multiple Arguments


;; Given Tests
(test/exn (parse-sexpr '{fun {f x x} x}) "parse-fun: bad syntax: (fun (f x x) x)")
(test (parse-sexpr '{fun {f x y} x}) (fun '(f x y) (id 'x)))

;;Given Test Cases
(test (run "{record {a 10} {b {+ 1 2}}}")
      'record)
(test (run "{getField {record {a 10} {b {+ 1 2}}} b}")
      3)
(test/exn (run "{getField {record {b 10} {b {+ 1 2}}} b}")
          "duplicate fields")
(test/exn (run "{getField {record {a 10}} b}")
          "no such field")
(test (run "{with {g {fun {r} {getField r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}")
      12)
(test (run "{getField {record {r {record {z 0}}}} r}")
      'record)
(test (run "{getField {getField {record {r {record {z 0}}}} r} z}")
      0)
(test/exn (run "{record {z {getField {record {z 0}} y}}}")
          "no such field")
(test (run "{with {f {fun {a b} {+ a b}}}
                  {with {g {fun {x} {- x 5}}}
                        {with {x {f 2 5}} {g x}}}}") 2)
(test (run "{with {f {fun {x y} {+ x y}}} {f 1 2}}") 3)
(test (run "{with {f {fun {} 5}}
                  {+ {f} {f}}}") 10)
(test (run "{with {h {fun {x y z w} {+ x w}}}
                  {h 1 4 5 6}}") 7) 
(test (run "{with {f {fun {} 4}}
                  {with {g {fun {x} {+ x x}}}
                        {with {x 10} {- {+ x {f}} {g 4}}}}}") 6)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{getField {record {r {record {z 0}}}} r}") 'record)
(test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {x 3} {with {y 5} {getField {record {a x} {b y}} a}}}") 3)
(test (run "{with {f {fun {a b} {+ {getField a a} b}}}
                  {with {g {fun {x} {+ 5 x}}}
                        {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}") 17)
(test (run "{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}}
                  {getField {f 1 2 3 4 5} c}}") 3)
(test (run "{with {f {fun {a b c} {record {a a} {b b} {c c}}}}
                  {getField {f 1 2 3} b}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {getField {f 1 2 3} y}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {getField {f 1 2 3} d}}") 2)
(test (run "{with {f {fun {x} {+ 5 x}}}
                  {f {getField {getField {record {a {record {a 10} {b {- 5 2}}}} {b {getField {record {x 50}} x}}} a} b}}}") 8)
(test (run "{getField {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{getField {record {r {record {z 0}}}} r}") 'record)
(test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
(test (run "{record {a 10}}") 'record)
(test (run "{getField {record {a 10}} a}") 10)
(test (run "{getField {record {a {+ 1 2}}} a}") 3)
(test (run "{fun {x} x}") 'function)
(test (run "{getField {record {a {record {b 10}}}} a}") 'record)
(test (run "{getField {getField {record {a {record {a 10}}}} a} a}") 10)
(test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{+ {getField {record {a 10}} a} {getField {record {a 20}} a}}") 30)
(test (run "{+ {getField {record {a 10}} a} {getField {record {a 20}} a}}") 30)
(test (run "{record {a 10}}") 'record)
(test (run "{record {a {- 2 1}}}") 'record)
(test (run "{getField {record {a 10}} a}") 10)
(test (run "{getField {record {a {- 2 1}}} a}") 1)
(test (run "{getField {record {a {record {b 10}}}} a}") 'record)
(test (run "{getField {getField {record {a {record {a 10}}}} a} a}") 10)
(test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{getField {getField {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{getField {record {r {record {z 0}}}} r}") 'record)
(test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {getField y y}}") 2)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {getField y z}}") 3)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{getField {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{with {g {fun {r} {getField r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}") 12)
(test (run "{getField {record {r {record {z 0}}}} r}") 'record)
(test (run "{getField {getField {record {r {record {z 0}}}} r} z}") 0)

;; Debugging
;(run "{record {a 10} {b {+ 1 2}}}")
;(parse "{getField {record {a 10} {b {+ 1 2}}} b}")
;(interp (parse "{record {a 10} {b {+ 1 2}}}") (mtSub))
;(interp (parse "{getField {record {a 10} {b {+ 1 2}}} b}") (mtSub))
;(run "{getField {record {a 10} {b {+ 1 2}}} b}")
;(run "{fun {x y z x} {+ x x}}")
