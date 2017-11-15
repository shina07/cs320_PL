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
  [num       (n number?)]
  [add       (lhs KCFAE?)
             (rhs KCFAE?)]
  [sub       (lhs KCFAE?)
             (rhs KCFAE?)]
  [id        (name symbol?)]
  [fun       (param (listof symbol?))
             (body KCFAE?)]
  [app       (fun-expr KCFAE?)
             (arg-expr (listof KCFAE?))]
  [if0       (test-expr KCFAE?)
             (then-expr KCFAE?)
             (else-expr KCFAE?)]
  [withcc    (name symbol?)
             (body KCFAE?)]
  [try-catch (try KCFAE?)
             (catch KCFAE?)]
  [throw]
)

(define-type KCFAE-Value
  [numV      (n number?)]
  [closureV  (params (listof symbol?))
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

;; parse : S-expr -> KCFAE
;; to convert s-expressions into KCFAEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (if (symbol=? 'throw sexp)
                        (throw)
                        (id sexp))]
    [(pair? sexp)
     (case (car sexp)
       [(+)       (add (parse (second sexp)) (parse (third sexp)))]
       [(-)       (sub (parse (second sexp)) (parse (third sexp)))]
       [(fun)     (fun (second sexp) (parse (third sexp)))]
       [(if0)     (if0 (parse (second sexp))
                       (parse (third sexp))
                       (parse (fourth sexp)))]
       [(withcc)  (withcc (second sexp) (parse (third sexp)))]
       [(try)     (try-catch (parse (second sexp)) (parse (fourth sexp)))]
       [else      (parse-app sexp)])]))

;; parse-app : S-expr (list-of-S-expr) -> KCFAE (list-of-KCFAE)
;;                                                    [Contract]

;; To parse the list of argument expression given     [Purpose]
;; in "app" constructor. Take the given app expression
;; and return KCFAE, with using "app" constructor
;; and parsed given arguments
(define (parse-app sexp)
  (local [(define args (map (lambda (x) (parse x)) (rest sexp)))]
    (app (parse (first sexp)) args)
  )
)

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

;; insert : (list-of-symbols) (list-of-KCFAE) DefrdSub -> DefrdSub
;;                                                      [Contract]

;; To add multiple number of symbols and values given   [Purpose]
;; in the app expression at the same time. 
(define (insert names values ds)
  (cond
    [(not (equal? (length names)
                  (length values))) (error 'insert "wrong arity")]
    [(empty? names) ds]
    [else  (insert (rest names)
                   (rest values)
                   (aSub (first names) (first values) ds))]
  )
)

;; interp: KCFAE DefrdSub (KCFAE-Value -> alpha) -> alpha
;; evaluates KCFAE expressions by reducing them to numbers
(define (interp kcfae ds k catch)
  (type-case KCFAE kcfae
    [num (n)      (k (numV n))]
    [add (l r)    (interp l ds
                          (lambda (v1)
                            (interp r ds
                                    (lambda (v2)
                                      (k (num+ v1 v2)))
                                    catch))
                          catch)]
    [sub (l r)    (interp l ds
                          (lambda (v1)
                            (interp r ds
                                    (lambda (v2)
                                      (k (num- v1 v2)))
                                    catch))
                          catch)]
    [id (s)       (k (lookup s ds))]
    [fun (params body-expr)
         (k (closureV params body-expr ds))]
    [app (fun-expr arg-exprs)
         (interp-app fun-expr arg-exprs ds k catch)]
    [if0 (test-expr then-expr else-expr)
                  (interp test-expr ds
                          (lambda (v)
                            (if (numzero? v)
                                (interp then-expr ds k catch)
                                (interp else-expr ds k catch)))
                          catch)]
    [withcc (id body)
                  (interp body
                          (aSub id
                                (contV k)
                                ds)
                          k
                          catch)]
    [try-catch (try-expr catch-expr)
               (interp try-expr ds k
                       (lambda ()
                         (interp catch-expr ds k catch)))]
    [throw ()
           (catch)]
  )
)

;; interp-app : KCFAE DefrdSub (KCFAE-Value -> alpha) ->alpha
;;                                                        [Contract]

;; To evaluate app, additional steps were required for    [Purpose]
;; multiple arguments. In order to evaluate variable
;; number of arguments, new type of interp function
;; is implemented 
(define (interp-app fun-expr arg-exprs ds k catch)
  (define (interp-args args ds k catch)
    (cond
      [(empty? args) (k empty)]
      [else          (interp (first args) ds
                             (lambda (v)
                               (interp-args (rest args)
                                            ds
                                            (lambda (vs) (k (cons v vs)))
                                            catch))
                             catch)]
    )
  )

  (interp fun-expr ds
          (lambda (fun-val)
            (interp-args arg-exprs ds
                         (lambda (arg-vals)
                           (type-case KCFAE-Value fun-val
                             [closureV (params body ds2)
                                       (cond
                                         [(equal? (length arg-vals) (length params))
                                          (interp body
                                                  (insert params
                                                          arg-vals
                                                          ds2)
                                                  k
                                                  catch)]
                                         [else     (error 'interp "wrong arity")]
                                       )]
                             [contV    (k)
                                       (k (first arg-vals))]
                             [else     (error 'interp "not a function")])
                           )
                         catch))
          catch
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
  (type-case KCFAE-Value (interp kcfae
                                 (mtSub)
                                 (lambda (x) x)
                                 (lambda () (error 'interp "undefined")))
    [numV     (n)             n]
    [closureV (params body ds)'function]
    [contV    (k)             'function]
  )
)

;; run : string -> num-or-'function
(define (run str)
  (interp-expr (parse (string->sexpr str)))
)

;; Given Test Cases
   ;; Parse
(test (parse 3) (num 3))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{withcc x 2}) (withcc 'x (num 2)))

   ;; Lookup
(test/exn (lookup 'x (mtSub)) "free variable")
(test (lookup 'x (aSub 'x (numV 9) (mtSub))) (numV 9))
(test (lookup 'x (aSub 'y (numV 10) (aSub 'x (numV 9) (mtSub)))) (numV 9))

   ;; Multiple Arguments
(test (run "{{fun {x y} {- y x}} 10 12}") 2)
(test (run "{fun {} 12}") 'function)
(test (run "{fun {x} {fun {} x}}") 'function)
(test (run "{{{fun {x} {fun {} x}} 13}}") 13)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)

   ;; More Test Cases
(test (run "{try 7 catch 8}")
        7)
  (test (run "{try {throw} catch 8}")
        8)
  (test (run "{try {+ 1 {throw}} catch 8}")
        8)
  (test (run "{{fun {f} {try {f 3} catch 8}}
               {fun {x} {throw}}}")
        8)
  (test (run "{try {try {throw} catch 8} catch 9}")
        8)
  (test (run "{try {try {throw} catch {throw}} catch 9}")
        9)
  (test (run "{try {try 7 catch {throw}} catch 9}")
        7)
  (test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}}
               {fun {x} {throw}}}")
	8)

; multiple arguments (5)
(test (run "{{fun {x y} {- y x}} 10 12}") 2)
(test (run "{fun {} 12}") 'function)
(test (run "{fun {x} {fun {} x}}") 'function)
(test (run "{{{fun {x} {fun {} x}} 13}}") 13)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)

; exceptions (35)
(test (run "{+ {withcc k {k 5}} 4}" ) 9)
(test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}") 55) ; recursive function
(test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}") 100) ; exit from recursive function using continuation
(test (run "{withcc k {- 0 {k 100}}}" ) 100)
(test (run "{withcc k {k {- 0 100}}}" ) -100)
(test (run "{withcc k {k {+ 100 11}}}" ) 111)
(test (run "{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}" ) 0)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)
(test (run "{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}") 20)
(test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}") 110) ; exit from recursive function using try-catch
(test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}") 54) ; equal? for multiple recursive try-catch
(test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}") 2)
(test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}") 20110464) ; recursive try-catch throwing (1)
(test (run "{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}") 0)
(test (run "{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}") 8)
(test (run "{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}") 89)
(test (run "{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}") 11)
(test (run "{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}") 5)
(test (run "{+ {try {- 10 {throw}} catch 3} 10}") 13)
(test (run "{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}") 54)
(test (run "{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}") 10)
(test (run "{try {- 0 {throw}} catch 5}") 5)
(test (run "{try {if0 {throw} 3 4} catch 5}") 5)
(test (run "{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}") -1)
(test (run "{try {try {throw} catch {throw}} catch 9}") 9)
(test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
(test (run "{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
(test (run "{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}") 5)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}") 10)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}") 42)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}") 3)
(test (run "{withcc esc {try {+ {throw} {esc 3}} catch 4}}") 4)
(test (run "{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}") 15)
(test (run "{try {withcc x {+ {x 1} {throw}}} catch 0}") 1)
(test (run "{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}") 19)

; multiple arguments (6)
(test (run "{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}") 1099)
(test (run "{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}") 11053)
(test (run "{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}") 0)
(test (run "{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}") 64)
(test (run "{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}") 'function)
(test (run "{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}}") 42)

; exceptions (4)
(test (run "{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}") 4242)
(test (run "{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}") 33)
(test (run "{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}") 0)
(test (run "{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}") 4242)


;; Private Test Cases
   ;; Insert
(test (insert '(x y) (list (numV 1)(numV 10)) (mtSub)) (aSub 'y (numV 10) (aSub 'x (numV 1) (mtSub))))
(test (insert '(x y z) (list (numV 1)(numV 2)(numV 3)) (mtSub)) (aSub 'z (numV 3) (aSub 'y (numV 2) (aSub 'x (numV 1) (mtSub)))))
(test/exn (insert '(x y) (list (numV 10)) (mtSub)) "wrong arity")

   ;; Multiple Arguments
      ;; parse
(test (parse '{fun {x} x}) (fun '(x) (id 'x)))
(test (parse '{1 2 2 3}) (app (num 1) (list (num 2) (num 2) (num 3))))
      ;; interp-expr
(test (interp-expr (try-catch (num 7) (num 8))) 7)
(test (interp-expr (fun '(x) (id 'x))) 'function)
(test (interp-expr (withcc 'x (id 'x))) 'function)
      ;; interp
(test (run "1") 1)
(test (run "{+ 1 2}") 3)
(test (run "{withcc k {+ 1 {k 2}}}") 2)
(test (run "{withcc k {+ {k 10} 17}}") 10)
(test (run "{withcc k {k 10}}") 10)
(test (run "{{fun {a} a}{withcc esc {{fun {x y} x} 1 {esc {{fun {x y} {- y x}} 10 12}} 10}}}") 2)
      ;; From Lecture Slides
(test (run "{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}") 4)

   ;; Exceptions
(test (run "{try {try {try {throw} catch 1} catch 2} catch 3}") 1)
(test (run "{try 3 catch 11}") 3)
(test/exn (run "{try {+ 0 {throw}} catch {throw}}") "interp: undefined")
