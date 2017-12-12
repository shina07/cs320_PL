#lang plai-typed

(define-type TRCFAE
  [num (n : number)]
  [add (lhs : TRCFAE) (rhs : TRCFAE)]
  [sub (lhs : TRCFAE) (rhs : TRCFAE)]
  [id  (name : symbol)]
  [fun (param : symbol) (type : TE) (body : TRCFAE)]
  [app (fun-expr : TRCFAE) (arg-expr : TRCFAE)]
  [if0 (test-expr : TRCFAE)
       (then-expr : TRCFAE)
       (else-expr : TRCFAE)]
  [rec (name : symbol)
       (ty : TE)
       (rhs-expr : TRCFAE)
       (body-expr : TRCFAE)]
)

(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (arg : TE) (result : TE)]
)

(define-type Type
  [numT]
  [boolT]
  [arrowT (arg : Type) (result : Type)]
)

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol) (type : Type) (rest : TypeEnv)]
)

(define-type DefrdSub
  [mtSub]
  [aSub    (name : symbol) (value : TRCFAE-Value)  (rest : DefrdSub)]
  [aRecSub (name : symbol) (value : (boxof TRCFAE-Value)) (ds : DefrdSub)]
)

(define-type TRCFAE-Value
  [numV     (n : number)]
  [closureV (param : symbol)
            (body : TRCFAE)
            (ds : DefrdSub)]
  [boolV    (b : boolean)]
)
;; -----------------------------------------------------------
;;                       Type Checking
;; -----------------------------------------------------------
(define typecheck : (TRCFAE TypeEnv -> Type)
  (lambda (trcfae env)
    (type-case TRCFAE trcfae
      [num (n) (numT)]
      [add (l r)
           (type-case Type (typecheck l env)
             [numT ()
                   (type-case Type (typecheck r env)
                     [numT () (numT)]
                     [else (type-error r "num")]
                   )]
             [else (type-error l "num")]
           )]
      [sub (l r)
           (type-case Type (typecheck l env)
             [numT ()
                   (type-case Type (typecheck r env)
                     [numT () (numT)]
                     [else (type-error r "num")]
                   )]
             [else (type-error l "num")]
           )]
      [id  (name) (get-type name env)]
      [fun (name te body)
           (local [(define param-type (parse-type te))]
             (arrowT param-type
                     (typecheck body (aBind name
                                            param-type
                                            env))))]
      [app (fn arg)
           (type-case Type (typecheck fn env)
             [arrowT (param-type result-type)
                     (if (equal? param-type
                                 (typecheck arg env))
                         result-type
                         (type-error arg
                                     (to-string param-type)))]
             [else (type-error fn "function")])]
      [if0 (test-expr then-expr else-expr)
           (type-case Type (typecheck test-expr env)
             [numT () (local [(define test-ty (typecheck then-expr env))]
                        (if (equal? test-ty (typecheck else-expr env))
                            test-ty
                            (type-error else-expr (to-string test-ty))))]
             [else (type-error test-expr "num")]
           )]
      [rec (name ty rhs-expr body-expr)
           (local [(define rhs-ty (parse-type ty))
                   (define new-env (aBind name rhs-ty env))]
             (if (equal? rhs-ty (typecheck rhs-expr new-env))
                 (typecheck body-expr new-env)
                 (type-error rhs-expr (to-string rhs-ty))))]
    )
  )
)

(define (get-type x env)
 (type-case TypeEnv env
   [mtEnv () (error 'get-type "free variable, no type")]
   [aBind (name type rest)
          (if (symbol=? x name)
              type
              (get-type x rest))]
 )
)

(define (parse-type te)
  (type-case TE te
    [numTE   ()    (numT)]
    [boolTE  ()    (boolT)]
    [arrowTE (a b) (arrowT (parse-type a)
                           (parse-type b))]
  )
)

(define (type-error trcfae msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string trcfae)
                      (string-append " not " msg)))
   )
)

;; -----------------------------------------------------------
;;                       Interpretation
;; -----------------------------------------------------------

;; interp : TRCFAE DefrdSub -> TRCFAE-Value
(define (interp trcfae ds)
  (type-case TRCFAE trcfae
    [num   (n)   (numV n)]
    [add   (l r) (num+ (interp l ds) (interp r ds))]
    [sub   (l r) (num- (interp l ds) (interp r ds))]
    [id    (s)   (lookup s ds)]
    [fun   (param param-te body-expr)
           (closureV param body-expr ds)]
    [app   (fun-expr arg-expr)
           (local [(define fun-val (interp fun-expr ds))
                   (define arg-val (interp arg-expr ds))]
             (interp (closureV-body fun-val)
                     (aSub (closureV-param fun-val)
                           arg-val
                           (closureV-ds fun-val))))]
    [if0   (test-expr then-expr else-expr)
           (if (numzero? (interp test-expr ds))
               (interp then-expr ds)
               (interp else-expr ds))]
    [rec   (bound-id type named-expr body-expr)
           (local [(define value-holder (box (numV 42)))
                   (define new-ds (aRecSub bound-id value-holder ds))]
             (begin
               (set-box! value-holder (interp named-expr new-ds))
               (interp body-expr new-ds))
           )]
  )
)

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub    () (error 'lookup "free variable")]
    [aSub     (sub-name value rest)
              (if (symbol=? sub-name name)
                  value
                  (lookup name rest))]
    [aRecSub  (sub-name val-box rest)
              (if (symbol=? sub-name name)
                  (unbox val-box)
                  (lookup name rest))]
  )
)

;; num-op : (number number -> number) -> (TRCFAE-Value TRCFAE-Value -> TRCFAE->Value)
(define (num-op op x y)
  (numV (op (numV-n x) (numV-n y)))
)

(define (numzero? n)
  (zero? (numV-n n))
)


(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

(define eval : (TRCFAE -> TRCFAE-Value)
  (lambda (trcfae)
    (begin
      (try (typecheck trcfae (mtEnv))
           (lambda () (error 'typecheck "type-error")))
      (interp trcfae (mtSub))
    )
  )
)


(test (interp (num 10)
              (mtSub))
      (numV 10))
(test (interp (add (num 10) (num 17))
              (mtSub))
      (numV 27))
(test (interp (sub (num 10) (num 7))
              (mtSub))
      (numV 3))
(test (interp (app (fun 'x (numTE) (add (id 'x) (num 12)))
                   (add (num 1) (num 17)))
              (mtSub))
      (numV 30))
(test (interp (id 'x)
              (aSub 'x (numV 10) (mtSub)))
      (numV 10))

(test (interp (app (fun 'x (numTE)
                        (app (fun 'f (arrowTE (numTE) (numTE))
                                  (add (app (id 'f) (num 1))
                                       (app (fun 'x (numTE)
                                                 (app (id 'f)
                                                      (num 2)))
                                            (num 3))))
                             (fun 'y (numTE)
                                  (add (id 'x) (id 'y)))))
                   (num 0))
              (mtSub))
      (numV 3))

(test (interp (if0 (num 0) (num 1) (num 2))
              (mtSub))
      (numV 1))
(test (interp (if0 (num 1) (num 1) (num 2))
              (mtSub))
      (numV 2))
(test (interp (rec 'a (numTE)
                (num 10)
                (add (id 'a) (num 1)))
              (mtSub))
      (numV 11))
(test (interp (rec 'fib (arrowTE (numTE) (numTE))
                (fun 'x (numTE)
                     (if0 (id' x)
                          (num 1)
                          (if0 (sub (id 'x) (num 1))
                               (num 1)
                               (add (app (id 'fib) (sub (id 'x) (num 1)))
                                    (app (id 'fib) (sub (id 'x) (num 2)))))))
                (app (id 'fib) (num 4)))
              (mtSub))
      (numV 5))

(test/exn (interp (id 'x) (mtSub))
          "free variable")

(test (typecheck (num 10) (mtEnv))
      (numT))

(test (typecheck (add (num 10) (num 17)) (mtEnv))
      (numT))
(test (typecheck (sub (num 10) (num 7)) (mtEnv))
      (numT))

(test (typecheck (fun 'x (numTE) (add (id 'x) (num 12))) (mtEnv))
      (arrowT (numT) (numT)))

(test (typecheck (fun 'x (numTE) (fun 'y (boolTE) (id 'x))) (mtEnv))
      (arrowT (numT) (arrowT (boolT)  (numT))))

(test (typecheck (app (fun 'x (numTE) (add (id 'x) (num 12)))
                      (add (num 1) (num 17)))
                 (mtEnv))
      (numT))

(test (typecheck (app (fun 'x (numTE)
                           (app (fun 'f (arrowTE (numTE) (numTE))
                                     (add (app (id 'f) (num 1))
                                          (app (fun 'x (numTE) (app (id 'f) (num 2)))
                                               (num 3))))
                                (fun 'y (numTE)
                                     (add (id 'x)
                                          (id' y)))))
                      (num 0))
                 (mtEnv))
      (numT))

(test (typecheck (if0 (num 0) (num 1) (num 2))
                 (mtEnv))
      (numT))
(test (typecheck (if0 (num 0)
                      (fun 'x (numTE) (id 'x))
                      (fun 'y (numTE) (num 3)))
                 (mtEnv))
      (arrowT (numT) (numT)))
(test (typecheck (rec 'a (numTE)
                   (num 10)
                   (add (id 'a) (num 1)))
                 (mtEnv))
      (numT))
(test (typecheck (rec 'fib (arrowTE (numTE) (numTE))
                   (fun 'x (numTE)
                        (if0 (id' x)
                             (num 1)
                             (if0 (sub (id 'x) (num 1))
                                  (num 1)
                                  (add (app (id 'fib) (sub (id 'x) (num 1)))
                                       (app (id 'fib) (sub (id 'x) (num 2)))))))
                   (app (id 'fib) (num 4)))
                 (mtEnv))
      (numT))

(test/exn (typecheck (app (num 1) (num 2)) (mtEnv))
          "no type")

(test/exn (typecheck (add (fun 'x (numTE) (num 12))
                          (num 2))
                     (mtEnv))
          "no type")
(test/exn (typecheck (if0 (num 0)
                          (num 7)
                          (fun 'y (numTE) (num 3)))
                     (mtEnv))
          "no type")
(test/exn (typecheck (rec 'x (numTE)
                       (fun 'y (numTE) (num 3))
                       (num 10))
                     (mtEnv))
          "no type")
(test/exn (typecheck (rec 'x (arrowTE (numTE) (numTE))
                       (fun 'y (numTE) (num 3))
                       (add (num 1) (id 'x)))
                     (mtEnv))
          "no type")