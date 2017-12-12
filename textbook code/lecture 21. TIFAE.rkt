#lang plai-typed

(define-type TIFAE
  [num (n : number)]
  [add (lhs : TIFAE) (rhs : TIFAE)]
  [sub (lhs : TIFAE) (rhs : TIFAE)]
  [id  (name : symbol)]
  [fun (param : symbol) (type : TE) (body : TIFAE)]
  [app (fun-expr : TIFAE) (arg-expr : TIFAE)]
  [if0 (test-expr : TIFAE)
       (then-expr : TIFAE)
       (else-expr : TIFAE)]
  [rec (name : symbol)
       (ty : TE)
       (rhs-expr : TIFAE)
       (body-expr : TIFAE)]
)

(define-type TE
  [numTE]
  [arrowTE (arg : TE) (result : TE)]
  [guessTE]
)

(define-type Type
  [numT]
  [arrowT (arg : Type) (result : Type)]
  [varT (is : (boxof (Option Type)))]
)

(define-type (Option 'alpha)
  [none]
  [some (v : 'alpha)]
)

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol) (type : Type) (rest : TypeEnv)]
)

(define-type DefrdSub
  [mtSub]
  [aSub    (name : symbol) (value : TIFAE-Value)  (rest : DefrdSub)]
  [aRecSub (name : symbol) (value : (boxof TIFAE-Value)) (ds : DefrdSub)]
)

(define-type TIFAE-Value
  [numV     (n : number)]
  [closureV (param : symbol)
            (body : TIFAE)
            (ds : DefrdSub)]
)

;; -----------------------------------------------------------
;;                       Interpretation
;; -----------------------------------------------------------

;; interp : TIFAE DefrdSub -> TIFAE-Value
(define (interp tifae ds)
  (type-case TIFAE tifae
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

;; num-op : (number number -> number) -> (TIFAE-Value TIFAE-Value -> TIFAE->Value)
(define (num-op op x y)
  (numV (op (numV-n x) (numV-n y)))
)

(define (numzero? n)
  (zero? (numV-n n))
)


(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

;; -----------------------------------------------------------
;;                       Type Checking
;; -----------------------------------------------------------
(define typecheck : (TIFAE TypeEnv -> Type)
  (lambda (tifae env)
    (type-case TIFAE tifae
      [num (n) (numT)]
      [add (l r) (begin
                   (unify! (typecheck l env) (numT) l)
                   (unify! (typecheck r env) (numT) r)
                   (numT)
                 )]
      [sub (l r) (begin
                   (unify! (typecheck l env) (numT) l)
                   (unify! (typecheck r env) (numT) r)
                   (numT)
                 ) ]
      [id  (name) (get-type name env)]
      [fun (name te body)
           (local [(define param-type (parse-type te))]
             (begin
               (validtype param-type env)
               (arrowT param-type
                       (typecheck body (aBind name
                                              param-type
                                              env)))
             ))]
      [app (fn arg)
           (local [(define result-type (varT (box (none))))]
             (begin
               (unify! (arrowT (typecheck arg env) result-type)
                       (typecheck fn env)
                       fn)
               result-type
             )
           )]
      [if0 (test-expr then-expr else-expr)
           (begin
             (unify! (typecheck test-expr env) (numT) test-expr)
             (local [(define test-ty (typecheck then-expr env))]
               (begin
                 (unify! test-ty (typecheck else-expr env) else-expr)
                 test-ty
               )
             )
           )]
      [rec (name ty rhs-expr body-expr)
           (local [(define rhs-ty (parse-type ty))
                   (define new-env (aBind name rhs-ty env))]
             (begin
               (validtype rhs-ty env)
               (unify! rhs-ty (typecheck rhs-expr new-env) rhs-expr)
               (typecheck body-expr new-env)
             )
           )]
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
    [arrowTE (a b) (arrowT (parse-type a)
                           (parse-type b))]
    [guessTE ()    (varT   (box (none)))]
  )
)

(define (validtype ty env)
  (type-case Type ty
    [numT   ()    (mtEnv)]
    [arrowT (a b) (begin (validtype a env)
                         (validtype b env))]
    [varT   (box) (mtEnv)]
  )
)

(define (type-error tifae t1 t2)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string tifae)
                      (string-append
                       " type "
                       (string-append
                        (to-string t1)
                        (string-append
                         " vs. "
                         (to-string t2))))))
   )
)

;; -----------------------------------------------------------
;;                       Type Unification
;; -----------------------------------------------------------
(define (unify! t1 t2 expr)
  (type-case Type t1
    [varT (is1) (type-case (Option Type) (unbox is1)
                  [some (t3) (unify! t3 t2 expr)]
                  [none ()   (local [(define t3 (resolve t2))]
                               (if (eq? t1 t3)
                                   (values)
                                   (if (occurs? t1 t3)
                                       (type-error expr t1 t3)
                                       (begin
                                         (set-box! is1 (some t3))
                                         (values)))))]
                )]
    [else (type-case Type t2
            [varT   (is2) (unify! t2 t1 expr)]
            [numT   ()    (type-case Type t1
                            [numT () (values)]
                            [else (type-error expr t1 t2)])]
            [arrowT (a2 b2)
                    (type-case Type t1
                      [arrowT (a1 b1)
                              (begin (unify! a1 a2 expr)
                                     (unify! b1 b2 expr))]
                      [else   (type-error expr t1 t2)]
                    )]
          )]
  )
)

(define (resolve t)
  (type-case Type t
    [varT (is) (type-case (Option Type) (unbox is)
                 [none ()   t]
                 [some (t2) (resolve t2)]
               )]
    [else t]
  )
)

(define (occurs? r t)
  (type-case Type t
    [numT   ()    false]
    [arrowT (a b) (or (occurs? r a) (occurs? r b))]
    [varT   (is)  (or (eq? r t)
                      (type-case (Option Type) (unbox is)
                        [none ()   false]
                        [some (t2) (occurs? r t2)]
                      ))]
  )
)

;; -----------------------------------------------------------
;;                       Evaluation
;; -----------------------------------------------------------
(define eval : (TIFAE -> TIFAE-Value)
  (lambda (tifae)
    (begin
      (try (typecheck tifae (mtEnv))
           (lambda () (error 'typecheck "type-error")))
      (interp tifae (mtSub))
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


(test/exn (interp (id 'x) (mtSub))
          "free variable")

(test (unify! (typecheck (num 10) (mtEnv))
              (numT)
              (num -1))
      (values))

(test (unify! (typecheck (add (num 10) (num 17)) (mtEnv))
              (numT)
              (num -1))
      (values))
(test (unify! (typecheck (sub (num 10) (num 7)) (mtEnv))
              (numT)
              (num -1))
      (values))

(test (unify! (typecheck (fun 'x (numTE) (add (id 'x) (num 12))) (mtEnv))
              (arrowT (numT) (numT))
              (num -1))
      (values))

(test (unify! (typecheck (app (fun 'x (numTE) (add (id 'x) (num 12)))
                              (add (num 1) (num 17)))
                         (mtEnv))
              (numT)
              (num -1))
      (values))

(test (unify! (typecheck (app (fun 'x (guessTE) (add (id 'x) (num 12)))
                              (add (num 1) (num 17)))
                         (mtEnv))
              (numT)
              (num -1))
      (values))

(test (unify! (typecheck (fun 'x (guessTE) (add (id 'x) (num 12)))
                         (mtEnv))
              (arrowT (numT) (numT))
              (num -1))
      (values))

(test (unify! (typecheck (fun 'x (guessTE) (if0 (num 0) (id 'x) (id 'x)))
                         (mtEnv))
              (arrowT (numT) (numT))
              (num -1))
      (values))

(test (unify! (typecheck (app (fun 'x (numTE)
                                   (app (fun 'f (arrowTE (numTE) (numTE))
                                             (add (app (id 'f) (num 1))
                                                  (app (fun 'x (numTE) (app (id 'f) (num 2)))
                                                       (num 3))))
                                        (fun 'y (numTE)
                                             (add (id 'x)
                                                  (id' y)))))
                              (num 0))
                         (mtEnv))
              (numT)
              (num -1))
      (values))

(test (unify! (typecheck (if0 (num 0) (num 1) (num 2))
                         (mtEnv))
              (numT)
              (num -1))
      (values))
(test (unify! (typecheck (if0 (num 0) 
                              (fun 'x (numTE) (id 'x))
                              (fun 'y (numTE) (num 3)))
                         (mtEnv))
              (arrowT (numT) (numT))
              (num -1))
      (values))


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

(test/exn (typecheck (fun 'x (guessTE) (app (id 'x) (id 'x)))
                     (mtEnv))
          "no type")