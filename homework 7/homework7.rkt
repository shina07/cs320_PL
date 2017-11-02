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
  [seqn    (exprs (listof BFAE?))]
  [rec     (records (listof Record?))]
  [get     (rec-expr BFAE?)(id symbol?)]
  [setrec  (rec-expr BFAE?)(id symbol?)(val BFAE?)]
)

(define-type BFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?)(body BFAE?)(ds DefrdSub?)]
  [boxV      (address integer?)]
  [recV      (records (listof Record->Addr?))]
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

(define-type Record
  [record (id symbol?)(expr BFAE?)]
)

(define-type Record->Addr
  [r*a (id symbol?)(address integer?)]
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
    [(list 'seqn e ...)        (seqn (map parse-sexpr e))]

    [(list 'rec r ...)         (parse-rec sexp)]
    [(list 'get e x)           (get (parse-sexpr e) x)]
    [(list 'set e x v)         (setrec (parse-sexpr e) x (parse-sexpr v))]

    ; app should be placed at the last line
    [(list f a)                (app (parse-sexpr f) (parse-sexpr a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

;; parse-rec : sexpr -> BFAE                            [Contract]

;; To check the error of "rec" constructor in parse     [Purpose]
;; phase. This code is inspired from the last homework
(define (parse-rec sexp)
  (local [(define records (rest sexp))]
    (if (check-duplicates (map (lambda (x) (first x)) records))
        (error 'parse "duplicate fields: ~a" sexp)
        (rec (map (lambda (x) (record (first x)(parse-sexpr (second x)))) records))
    )
  )
)

(test (parse-rec '{rec {x 1} {y 2}}) (rec (list (record 'x (num 1)) (record 'y (num 2)))))
(test/exn (parse-rec '{rec {x 1} {x 2}}) "duplicate fields")

;; parse : s-expression -> BFAE                         [Contract]

;; To convert s-expression into BFAE, just as the same  [Purpose]
;; as the intention of "parser". From this assignment,
;; the input type in the test cases are not string but
;; s-expression, so we should change this part as well
(define (parse sexpr)
  (parse-sexpr sexpr))

;; parse-str : string -> BFAE                           [Contract]

;; To convert string of s-exression into BFAE           [Purpose]
;; expression. This function is the "parse" function
;; until previous homework. 
(define (parse-str str)
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

;; store-lookup integer Store -> BFAE-Value
(define (store-lookup addr st)
  (type-case Store st
    [mtSto () (error 'store-lookup "unallocated")]
    [aSto (i val rest)
          (if (equal? i addr)
              val
              (store-lookup addr rest))]
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

;; modify-address : integer BFAE-Value Store -> Store       [Contract]

;; To improve setbox in the way to drop the old value and   [Purpose]
;; replace it into the new value, this function is
;; implemented by accepting the address, value, and the
;; store, and return the store that the value at that
;; given address is replaced
(define (modify-address addr value store)
  (type-case Store store
    [mtSto ()        (error 'modify-address "no such address: ~a" addr)]
    [aSto  (n v st)  (if (equal? n addr)
                         (aSto n value st)
                         (aSto n v (modify-address addr value st)))]))



;; record-lookup (list-of-Record) -> BFAE-Value            [Contract]

;; To find out the record that has the same id with the    [Purpose]
;; name in the given argument.
(define (record-lookup name records store)
  (if (empty? records)
      (error 'record-lookup "no such field: ~a" name)
      (if (equal? name (r*a-id (first records)))
          (store-lookup (r*a-address (first records)) store)
          (record-lookup name (rest records) store)
      )
  )
)

;; record-modify : (list-of-Record) -> BFAE-Value         [Contract]

;; To modify the value of the record that has the same id [Purpose]
;; with the name in the given argument.
(define (modify-record name value records store)
  (if (empty? records)
      (error 'record-modify "no such field: ~a" name)
      (if (equal? name (r*a-id (first records)))
          (modify-address (r*a-address (first records)) value store)
          (modify-record name value (rest records) store)
      )
  )
)

;; interp: BFAE DefrdSub Store -> Value*Store
;; evaluates BFAE expressions by reducing them to numbers
(define (interp bfae ds st)
  (type-case BFAE bfae
    [num (n)         (v*s (numV n) st)]
    [add (l r)       (interp-two l r ds st
                                 (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r)       (interp-two l r ds st
                                 (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id (s)          (v*s (lookup s ds) st)]
    [fun (x b)       (v*s (closureV x b ds) st)]
    [app (f a)       (interp-two f a ds st
                                 (lambda (f-val a-val st1)
                                   (interp (closureV-body f-val)
                                           (aSub (closureV-param f-val)
                                                 a-val
                                                 (closureV-ds f-val))
                                           st1)))]
    [newbox (v)      (type-case Value*Store (interp v ds st)
                       [v*s (v1 st1)
                            (local [(define a (malloc st1))]
                              (v*s (boxV a)
                                   (aSto a v1 st1)))])]
    [setbox (b v)    (interp-two b v ds st
                                 (lambda (b-val v-val st1)
                                   (v*s v-val
                                        (modify-address (boxV-address b-val)
                                                        v-val
                                                        st1))))]
    [openbox (b)     (type-case Value*Store (interp b ds st)
                       [v*s (b-val st1)
                            (v*s (store-lookup (boxV-address b-val)
                                               st1)
                                 st1)])]
    [seqn (e)        (interp-seqn e ds st)]
    [rec (r)         (interp-rec r ds st)]
    [get (e x)       (type-case Value*Store (interp e ds st)
                       [v*s (v1 st1)
                            (v*s (record-lookup x (recV-records v1) st1) st1)])]
    [setrec (e x v)  (interp-two e v ds st
                                 (lambda (e-val v-val st1)
                                   (v*s v-val
                                        (modify-record x v-val (recV-records e-val) st1))))]
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

;; interp-seqn : (list-of-BFAE) DefrdSub Store -> Value*Store
;;                                                        [Contract]

;; To evaluate sequences of expression in "seqn"          [Purpose]
;; constructor, new secondary interpreter is implemented,
;; since interp-two is only working for two-steps
;; evaluation
(define (interp-seqn exprs ds st)
  (type-case Value*Store (interp (first exprs) ds st)
    [v*s (v st2) (if (empty? (rest exprs))
                     (v*s v st2)
                     (interp-seqn (rest exprs) ds st2)
                 )]
  )
  
)

;; interp-rec : (list-of-Record) DefrdSub Store -> Value*Store
;;                                                       [Contract]

;; To evaluate records inside the expression of "rec"    [Purpose]
;; constructor, new secondary interpreter is implemented.
;; In order to interp whole record list, it is not enough
;; with using interp-two
(define (interp-rec records ds st)
  ;; interp-rec-two : Record (list of (list-of-Record->Addr) and (list-of-Store))
  ;;                      -> (list of (list-of-Record->Addr) and (list-of-Store))
  ;;                                                     [Contract]

  ;; In order to evaluate BFAE inside the records and    [Purpose]
  ;; pass store that contains all the evaluation of
  ;; those record, interp-rec-two is implemented in the
  ;; form of recursion with passing arguments
  (define (interp-rec-two rec acc-ra-st)
    (local [(define acc-rec->addr (first acc-ra-st))
            (define acc-store     (second acc-ra-st))]
      (type-case Record rec
        [record (s e)
                (type-case Value*Store (interp e ds acc-store)
                  [v*s (v1 st1)
                       (local [(define a (malloc st1))]
                         (list
                           (append acc-rec->addr (list (r*a s a)))
                           (aSto a v1 st1)
                         )
                       )]
                )]
      )
    )
  )
  (define (interp-rec-three acc-ra-st records)
    (if (empty? records)
        acc-ra-st
        (local [(define ra-st (interp-rec-two (first records) acc-ra-st))]
          (if (empty? (rest records))
              ra-st
              (interp-rec-three ra-st (rest records))
          )
        )
    )
  )

  (local [(define ra-st (interp-rec-three (list empty st) records))]
    (v*s (recV (first ra-st)) (second ra-st))
  )
)

; run : string -> Value*Store
;; evaluate a BFAE program contained in a string
(define (run str)
  (interp (parse-str str) (mtSub) (mtSto)))

;; interp-expr : BFAE -> (number, 'func, 'box, or 'record)    [Contract]

;; In order to evaluate BFAE in the usual way of interp,      [Purpose]
;; without returning the store. There are 4 return type
;; (just the same as BFAE-Value), but it will be returned
;; just as they are.
(define (interp-expr expr)
  (type-case Value*Store (interp expr (mtSub) (mtSto))
    [v*s (v s)
         (type-case BFAE-Value v
           [numV     (n)       n]
           [closureV (x b ds)  'func]
           [boxV     (a)       'box]
           [recV     (records) 'record]
         )]
  )
)

;; Testing interp-expr 
(test (interp-expr (parse '1)) 1)
(test (interp-expr (parse '{fun {x} x})) 'func)
(test (interp-expr (parse '{newbox 0})) 'box)
(test (interp-expr (parse '{rec})) 'record)

;; Given Tests
   ; Improving Assignments
(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b 2}
                           {openbox b}}}
                         {newbox 1}})
                (mtSub)
                (mtSto))
        (v*s (numV 2)
             (aSto 1 (numV 2) (mtSto))))
             
   ; Improving Sequences
(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b {+ 2 {openbox b}}}
                           {setbox b {+ 3 {openbox b}}}
                           {setbox b {+ 4 {openbox b}}}
                           {openbox b}}}
                         {newbox 1}})
                (mtSub)
                (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))

   ; Records
(test (interp-expr (parse '{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}})) 5)

;; Examples (More Given Test Cases)
(test (interp (parse '{seqn 1 2})
              (mtSub)
              (mtSto))
      (v*s (numV 2) (mtSto)))

(test (interp (parse '{{fun {b} {openbox b}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 1 (numV 10) (mtSto))))

(test (interp (parse '{{fun {b} {seqn
                                 {setbox b 12}
                                 {openbox b}}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 12)
           (aSto 1
                 (numV 12)
                 (mtSto))))

(test (interp-expr (parse '{{fun {b} {seqn
                                      {setbox b 12}
                                      {openbox b}}}
                            {newbox 10}}))
      12)

(test (interp (parse '{{fun {b} {openbox b}}
                       {seqn
                        {newbox 9}
                        {newbox 10}}})
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 2 (numV 10)
                 (aSto 1 (numV 9) (mtSto)))))

(test (interp (parse '{{{fun {b}
                             {fun {a}
                                  {openbox b}}}
                        {newbox 9}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 9)
           (aSto 2 (numV 10)
                 (aSto 1 (numV 9) (mtSto)))))
(test (interp (parse '{{fun {b}
                            {seqn
                             {setbox b 2}
                             {openbox b}}}
                       {newbox 1}})
              (mtSub)
              (mtSto))
      (v*s (numV 2)
           (aSto 1 (numV 2) (mtSto))))

(test (interp (parse '{{fun {b}
                            {seqn
                             {setbox b {+ 2 (openbox b)}}
                             {setbox b {+ 3 (openbox b)}}
                             {setbox b {+ 4 (openbox b)}}
                             {openbox b}}}
                       {newbox 1}})
              (mtSub)
              (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))


(test/exn (interp (parse '{openbox x})
                  (aSub 'x (boxV 1) (mtSub))
                  (mtSto))
          "unallocated")

;; records

(test (interp-expr (parse '{{fun {r}
                                 {get r x}}
                            {rec {x 1}}}))
      1)

(test (interp-expr (parse '{{fun {r}
                                 {seqn
                                  {set r x 5}
                                  {get r x}}}
                            {rec {x 1}}}))
      5)
(test (interp-expr (parse '{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}}            ; g
                              {fun {r} {fun {v} {set r b v}}}} ; s
                             {rec {a 0} {b 2}}}                ; r1
                            {rec {a 3} {b 4}}}))               ; r2
      5)

(test (interp-expr (parse '{fun {x} x}))
      'func)
(test (interp-expr (parse '{newbox 1}))
      'box)
(test (interp-expr (parse '{rec}))
      'record)

(test (interp (parse '{{fun {b} {setbox b 2}} {seqn {newbox 0} {newbox 1}}}) (mtSub) (mtSto)) (v*s (numV 2) (aSto 2 (numV 2) (aSto 1 (numV 0) (mtSto)))))
(test (interp (parse '{{fun {b} {setbox b 2}} {newbox 0}}) (mtSub) (aSto 1 (numV 0) (mtSto))) (v*s (numV 2) (aSto 2 (numV 2) (aSto 1 (numV 0) (mtSto)))))
(test (interp (parse '{{{fun {a} {fun {b} {setbox a 2}}} {newbox 1}} {newbox 0}}) (mtSub) (mtSto)) (v*s (numV 2) (aSto 2 (numV 0) (aSto 1 (numV 2) (mtSto)))))
(test (interp (parse '{+ {{fun {b} {setbox b 2}} {newbox 0}} {{fun {b} {setbox b 2}} {newbox 1}}}) (mtSub) (mtSto)) (v*s (numV 4) (aSto 2 (numV 2) (aSto 1 (numV 2) (mtSto)))))
(test (interp (parse '{newbox {{fun {b} {setbox b 2}} {newbox 0}}}) (mtSub) (mtSto)) (v*s (boxV 2) (aSto 2 (numV 2) (aSto 1 (numV 2) (mtSto)))))
(test (interp (parse '{openbox {{fun {b} {seqn {setbox b 2} {newbox {fun {a} {setbox a 3}}}}} {newbox 0}}}) (mtSub) (mtSto)) (v*s (closureV 'a (setbox (id 'a) (num 3)) (aSub 'b (boxV 1) (mtSub))) (aSto 2 (closureV 'a (setbox (id 'a) (num 3)) (aSub 'b (boxV 1) (mtSub))) (aSto 1 (numV 2) (mtSto)))))
(test (interp (parse '{{openbox {{fun {b} {seqn {setbox b 2} {newbox {fun {a} {setbox a 3}}}}} {newbox 0}}} {newbox 1}}) (mtSub) (mtSto)) (v*s (numV 3) (aSto 3 (numV 3) (aSto 2 (closureV 'a (setbox (id 'a) (num 3)) (aSub 'b (boxV 1) (mtSub))) (aSto 1 (numV 2) (mtSto))))))
(test (interp (parse '{seqn {newbox 0} {setbox x 1} {openbox x}}) (aSub 'x (boxV 1) (mtSub)) (aSto 1 (numV 0) (mtSto))) (v*s (numV 1) (aSto 2 (numV 0) (aSto 1 (numV 1) (mtSto)))))
(test (interp (parse '{{fun {b} {+ {openbox b} {seqn {setbox b 2} {openbox b}}}} {newbox 1}}) (mtSub) (mtSto)) (v*s (numV 3) (aSto 1 (numV 2) (mtSto))))
(test (interp (parse '{{fun {a} {{fun {b} {seqn {setbox b {- {openbox b} 1}} {setbox a {+ {openbox a} 1}} {newbox 0} {openbox b}}} {newbox 1}}} {newbox 2}}) (aSub 'a (boxV 0) (mtSub)) (mtSto)) (v*s (numV 0) (aSto 3 (numV 0) (aSto 2 (numV 0) (aSto 1 (numV 3) (mtSto))))))
(test (interp (parse '{seqn {newbox 1}}) (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (numV 1) (mtSto))))
(test (interp (parse '{setbox {{fun {b} {seqn {newbox b} {newbox b}}} 0} 1}) (mtSub) (mtSto)) (v*s (numV 1) (aSto 2 (numV 1) (aSto 1 (numV 0) (mtSto)))))
(test (interp (parse '{{fun {a} {{fun {b} {seqn {setbox b 2} {setbox a {fun {c} {+ c 1}}} {{openbox a} {openbox b}}}} {openbox a}}} {newbox {newbox 1}}}) (mtSub) (mtSto)) (v*s (numV 3) (aSto 2 (closureV 'c (add (id 'c) (num 1)) (aSub 'b (boxV 1) (aSub 'a (boxV 2) (mtSub)))) (aSto 1 (numV 2) (mtSto)))))
(test (interp (parse '{seqn 1 {fun {x} {+ x 1}} {newbox 2} {{fun {x} {setbox x {newbox 1}}} {newbox 3}}}) (mtSub) (mtSto)) (v*s (boxV 3) (aSto 3 (numV 1) (aSto 2 (boxV 3) (aSto 1 (numV 2) (mtSto))))))
(test (interp (parse '{{fun {b} {seqn {setbox b b} {openbox b}}} {newbox 0}}) (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (boxV 1) (mtSto))))
(test (interp (parse '{{fun {b} {openbox {setbox b b}}} {newbox 0}}) (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (boxV 1) (mtSto))))
(test (interp (parse '{{fun {b} {- {openbox b} {seqn {setbox b b} {setbox {openbox b} 1} {openbox b}}}} {newbox 0}}) (mtSub) (mtSto)) (v*s (numV -1) (aSto 1 (numV 1) (mtSto))))
(test (interp-expr (parse '{{fun {b} {{fun {a} {seqn {set a x {openbox b}} {setbox b 1} {set a y {openbox b}} {get a x}}} {rec {x 1} {y 2}}}} {newbox 0}})) 0)
(test (interp-expr (parse '{set {rec {x 1}} x 0})) 0)
(test (interp-expr (parse '{{fun {r} {seqn {setbox {get r x} 1} {get r y}}} {{fun {b} {rec {x b} {y {openbox b}}}} {newbox 0}}})) 0)
(test (interp-expr (parse '{{fun {r} {seqn {setbox {get r x} 1} {get r y}}} {{fun {b} {rec {x b} {y {openbox b}}}} {newbox 0}}})) 0)
(test (interp-expr (parse '{{fun {r1} {{fun {r} {seqn {set r x 0} {get r1 x}}} {rec {x 1} {y 2}}}} {rec {x 3} {y 4}}})) 3)
(test (interp-expr (parse '{{fun {r} {+ {get r x} {seqn {set r x 2}}}} {rec {z 3} {y 2} {x 1}}})) 3)
(test (interp-expr (parse '{{fun {b} {seqn {set {openbox b} y {newbox {openbox b}}} {openbox {get {openbox b} y}}}} {newbox {rec {x 1} {y 2}}}})) 'record)
(test (interp-expr (parse '{{fun {b} {seqn {set {openbox b} y {newbox {openbox b}}} {get {openbox {get {openbox b} y}} y}}} {newbox {rec {x 1} {y 2}}}})) 'box)
(test (interp-expr (parse '{{fun {r} {seqn {setbox {get r x} 2} {openbox {get r x}}}} {rec {x {newbox 0}}}})) 2)
(test (interp-expr (parse '{{fun {r} {seqn {setbox {get r x} 2} {openbox {get r x}}}} {rec {x {newbox 0}}}})) 2)
(test (interp-expr (parse '{{fun {r} {+ {setbox {get r x} 2} {openbox {get r x}}}} {rec {x {newbox 0}}}})) 4)

;; Private Testing

   ; During Initial Implementation
(run "1")
(run "{+ 1 2}")
(parse-str "{with {x {+ 5 5}} {+ x x}}")
(run "{with {x {+ 5 5}} {+ x x}}")

   ; Improving Assignment

   ; Improving Sequences
(parse '{seqn 0 1 2})

   ; Records
(check-duplicates (map (lambda (x) (first x)) '((x 1))))
(parse '{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}})

   ; These two tests are given in the Lecture Slide Tests
(run "{with {b {newbox 0}}{seqn {setbox b 10}{openbox b}}}")
(run "{with {q {newbox 10}}{setbox {seqn {setbox q 12} q}{openbox q}}}")


;; Debugging...


;(test (interp-expr (parse '{{{{{fun {g}
;                                    {fun {s}
;                                         {fun {r1}
;                                              {fun {r2}
;                                                   {+ {get r1 b}
;                                                      {seqn
;                                                       {{s r1} {g r2}}
;                                                       {+ {seqn
;                                                           {{s r2} {g r1}}
;                                                           {get r1 b}}
;                                                          {get r2 b}}}}}}}}
;                               {fun {r} {get r a}}}            ; g
;                              {fun {r} {fun {v} {set r b v}}}} ; s
;                             {rec {a 0} {b 2}}}                ; r1
;                            {rec {a 3} {b 4}}}))               ; r2
;      5)


;(parse '((fun (r) (seqn (setbox (get r x) 1) (get r y))) ((fun (b) (rec (x b) (y (openbox b)))) (newbox 0))))

;(interp (parse '(fun (r) (seqn (setbox (get r x) 1) (get r y)))) (mtSub) (mtSto))
;(interp (parse  '((fun (b) (rec (x b) (y (openbox b)))) (newbox 0))) (mtSub) (mtSto))

;(interp (parse '((fun (b) (rec (x b) (y b) (z b))) (newbox 0)))(mtSub) (mtSto))
;(interp-expr (parse '((fun (r) (seqn (setbox (get r x) 1) (get r y))) ((fun (b) (rec (x b) (y (openbox b)))) (newbox 0)))))

;(interp-expr (parse '{rec}))


;(test (interp (parse '{{fun {b} {- {openbox b} {seqn {setbox b b} {setbox {openbox b} 1} {openbox b}}}} {newbox 0}}) (mtSub) (mtSto)) (v*s (numV -1) (aSto 1 (numV 1) (mtSto))))
;(parse '{{fun {b} {- {openbox b} {seqn {setbox b b} {setbox {openbox b} 1} {openbox b}}}} {newbox 0}})

;(interp (parse '{{fun {b} {seqn {setbox b b} {setbox {openbox b} 1} {openbox b}}} {newbox 0}}) (mtSub) (mtSto))

;(interp (parse '{{fun {b} {- {openbox b} {seqn {setbox b b} {setbox {openbox b} 1} {openbox b}}}} {newbox 0}}) (mtSub) (mtSto))
