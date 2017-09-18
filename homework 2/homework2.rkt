#lang plai

; WAE type is from lecture
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?)
        (name-expr WAE?)
        (body WAE?)]
  [id (name symbol?)]
)

; symbol<?: symbol symbol -> string              [Contract]

; To find out whether string "a" should          [Purpose]
; come before string "b". First, symbols
; are changed into string, and then compared
; their alphanumeric order

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b))
)

; free-ids : WAE -> list-of-sym                  [Contract]

; To find out all the free identifiers inside    [Purpose]
; the given WAE. Free identifiers does not
; shows in the "name" field of "with" variant.
; Otherwise, we should add all single "id"s.

(define (free-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [add (l r) (sort (remove-duplicates (append (free-ids l) (free-ids r))) symbol<?)]
    [sub (l r) (sort (remove-duplicates (append (free-ids l) (free-ids r))) symbol<?)]
    [with (x i b) (sort (remove-duplicates (append (free-ids i) (remove x (free-ids b)))) symbol<?)]
    [id (x) (list x)]
  )
)

(test (free-ids (num 7)) empty)                       ;[Tests]
(test (free-ids (id 'x)) '(x))
(test (free-ids (sub (id 'x) (num 16))) '(x))
(test (free-ids (add (num 3) (id 'x))) '(x))
(test (free-ids (with 'x (with 'y (num 3) (add (id 'x) (num 1))) (add (with 'z (num 3) (add (id 'z) (id 'x))) (id 'x)))) '(x))
(test (free-ids (add (with 'x (num 1) (with 'x (id 'u) (sub (num 5) (with 'z (num 2) (add (id 'x) (sub (id 'v) (id 'b))))))) (with 'v (num 2) (add (id 'x) (sub (num 3) (id 'u)))))) '(b u v x))


; binding-ids : WAE -> list-of-sym               [Contract]

; To find out all the binding identifier in      [Purpose]
; the given WAE. In WAE, only the binding
; occurrence takes place in "name" field of
; "with" variant, so that is the only place
; we need to add for our result list

(define (binding-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [add (l r) (sort (remove-duplicates (append (binding-ids l) (binding-ids r))) symbol<?)]
    [sub (l r) (sort (remove-duplicates (append (binding-ids l) (binding-ids r))) symbol<?)]
    [with (x i b) (sort (remove-duplicates (cons x(append (binding-ids i)(binding-ids b)))) symbol<?)]
    [id (x) empty]
  )
)

(test (binding-ids (num 7)) '())
(test (binding-ids (add (id 'z) (id 'y))) '())
(test (binding-ids (with 'x (num 7) (id 'x))) '(x))
(test (binding-ids (with 'x (num 7) (add (num 15) (id 'x)))) '(x))
(test (binding-ids (with 'x (num 7) (with 'x (num 3) (id 'x)))) '(x))
(test (binding-ids (with 'x (with 'z (num 3) (id 'z)) (add (num 3) (with 'y (num 3) (id 'x))))) '(x y z))


; bound-ids : WAE -> list-of-sym                 [Contract]

; To find out all the bound identifier in        [Purpose]
; the given WAE. In body expression of "with"
; variant, if there exists given x in the list
; of free identifiers in body expression, that
; means it is bound occurrence.

(define (bound-ids wae)
  (type-case WAE wae
    [num (n) empty]
    [add (l r) (sort (remove-duplicates (append (bound-ids l) (bound-ids r))) symbol<?)]
    [sub (l r) (sort (remove-duplicates (append (bound-ids l) (bound-ids r))) symbol<?)]
    [with (x i b) (sort (remove-duplicates (cond
                                             [(member x (free-ids b))
                                                      (append (list x) (append (bound-ids i)(bound-ids b)))]
                                             [else (append (bound-ids i)(bound-ids b))])) symbol<?)]
    [id (x) empty]
  )
)

(test (bound-ids (with 'z (num 7) (id 'z))) '(z))
(test (bound-ids (with `x (num 1) (with `y (num 3) (id `x)))) '(x))
(test (bound-ids (add (id 'x) (with 'x (sub (num 3) (id 'y)) (id 'x)))) '(x))
(test (bound-ids (sub (id 'x) (with 'x (num 3) (id 'y)))) '())
(test (bound-ids (with 'a (num 1) (with 'b (num 2) (with 'c (num 3) (with 'd (num 4) (with 'e (num 5) (add (id 'a)(add (id 'b) (add (id 'c) (add (id 'd)(id 'e))))))))))) '(a b c d e))

;; Given Test Cases
;; free-ids
(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))

;; binding-ids
(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))
; bound-ids
(test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
(test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
(test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
(test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
(test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
(test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))
