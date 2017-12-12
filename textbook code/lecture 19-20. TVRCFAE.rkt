#lang plai-typed

(define-type TVRCFAE
  [num       (n : number)]
  [add       (lhs : TVRCFAE) (rhs : TVRCFAE)]
  [sub       (lhs : TVRCFAE) (rhs : TVRCFAE)]
  [id        (name : symbol)]
  [fun       (param : symbol) (type : TE) (body : TVRCFAE)]
  [app       (fun-expr : TVRCFAE) (arg-expr : TVRCFAE)]
  [if0       (test-expr : TVRCFAE)
             (then-expr : TVRCFAE)
             (else-expr : TVRCFAE)]
  [rec       (name : symbol)
             (ty : TE)
             (rhs-expr : TVRCFAE)
             (body-expr : TVRCFAE)]
  [with-type (name : symbol)
             (var1-name : symbol) (var1-ty : TE)
             (var2-name : symbol) (var2-ty : TE)
             (body-expr : TVRCFAE)]
  [cases     (name : symbol)
             (dispatch-expr : TVRCFAE)
             (var1-name : symbol) (bind1-name : symbol) (rhs1-expr : TVRCFAE)
             (var2-name : symbol) (bind2-name : symbol) (rhs2-expr : TVRCFAE)]
)

(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (arg : TE) (result : TE)]
  [idTE    (name : symbol)]
)

(define-type Type
  [numT]
  [boolT]
  [arrowT (arg : Type) (result : Type)]
  [idT    (name : symbol)]
)

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol) (type : Type) (rest : TypeEnv)]
  [tBind (name : symbol)
         (var1-name : symbol) (var1-type : Type)
         (var2-name : symbol) (var2-type : Type)
         (rest : TypeEnv)]
)

(define-type DefrdSub
  [mtSub]
  [aSub    (name : symbol) (value : TVRCFAE-Value)  (rest : DefrdSub)]
  [aRecSub (name : symbol) (value : (boxof TVRCFAE-Value)) (ds : DefrdSub)]
)

(define-type TVRCFAE-Value
  [numV         (n : number)]
  [closureV     (param : symbol)
                (body : TVRCFAE)
                (ds : DefrdSub)]
  [constructorV (right? : boolean)]
  [variantV     (right? : boolean) (val : TVRCFAE-Value)]
)


;; -----------------------------------------------------------
;;                       Interpretation
;; -----------------------------------------------------------

;; interp : TVRCFAE DefrdSub -> TVRCFAE-Value
(define (interp tvrcfae ds)
  (type-case TVRCFAE tvrcfae
    [num   (n)   (numV n)]
    [add   (l r) (num+ (interp l ds) (interp r ds))]
    [sub   (l r) (num- (interp l ds) (interp r ds))]
    [id    (s)   (lookup s ds)]
    [fun   (param param-te body-expr)
           (closureV param body-expr ds)]
    [app   (fun-expr arg-expr)
           (local [(define fun-val (interp fun-expr ds))
                   (define arg-val (interp arg-expr ds))]
             (type-case TVRCFAE-Value fun-val
               [closureV     (param body new-ds)
                             (interp body (aSub param arg-val new-ds))]
               [constructorV (right?)
                             (variantV right? arg-val)]
               [else (error 'interp "not applicable")]
             ))]
    [if0   (test-expr then-expr else-expr)
           (if (numzero? (interp test-expr ds))
               (interp then-expr ds)
               (interp else-expr ds))]
    [rec   (bound-id type named-expr body-expr)
           (local [(define value-holder (box (numV 42)))
                   (define new-ds (aRecSub bound-id value-holder ds))]
             (begin
               (set-box! value-holder (interp named-expr new-ds))
               (interp body-expr new-ds)))]
    [with-type (type-name var1-name var1-te
                            var2-name var2-te
                            body-expr)
                 (interp body-expr
                         (aSub var1-name
                               (constructorV false)
                               (aSub var2-name
                                     (constructorV true)
                                     ds)))]
    [cases     (ty dispatch-expr
                   var1-name var1-id var1-rhs
                   var2-name var2-id var2-rhs)
      (type-case TVRCFAE-Value (interp dispatch-expr ds)
        [variantV (right? val)
                  (if (not right?)
                      (interp var1-rhs (aSub var1-id val ds))
                      (interp var2-rhs (aSub var2-id val ds)))]
        [else (error 'interp "not a variant result")]
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

;; num-op : (number number -> number) -> (TVRCFAE-Value TVRCFAE-Value -> TVRCFAE->Value)
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
(define typecheck : (TVRCFAE TypeEnv -> Type)
  (lambda (tvrcfae env)
    (type-case TVRCFAE tvrcfae
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
             (begin
               (validtype param-type env)
               (arrowT param-type
                       (typecheck body (aBind name
                                              param-type
                                              env)))
             ))]
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
      [with-type (type-name var1-name var1-te var2-name var2-te body-expr)
        (local [(define var1-ty (parse-type var1-te))
                (define var2-ty (parse-type var2-te))
                (define new-env (tBind type-name
                                       var1-name var1-ty
                                       var2-name var2-ty env))]
          (begin
            (validtype var1-ty new-env)
            (validtype var2-ty new-env)
            (typecheck body-expr
                       (aBind var1-name
                              (arrowT var1-ty
                                      (idT type-name))
                              (aBind var2-name
                                     (arrowT var2-ty
                                             (idT type-name))
                                     new-env)))
          ))]
      [cases (type-name dispatch-expr var1-name var1-id var1-rhs
                                      var2-name var2-id var2-rhs)
        (local [(define bind (find-type-id type-name env))]
          (if (and (equal? var1-name (tBind-var1-name bind))
                   (equal? var2-name (tBind-var2-name bind)))
              (type-case Type (typecheck dispatch-expr env)
                [idT (name)
                     (if (equal? name type-name)
                         (local [(define rhs1-ty
                                   (typecheck var1-rhs
                                              (aBind var1-id (tBind-var1-type bind) env)))
                                 (define rhs2-ty
                                   (typecheck var2-rhs
                                              (aBind var2-id (tBind-var2-type bind) env)))]
                           (if (equal? rhs1-ty rhs2-ty)
                               rhs1-ty
                               (type-error var2-rhs (to-string rhs1-ty))))
                         (type-error dispatch-expr (to-string type-name)))]
                [else (type-error dispatch-expr (to-string type-name))])
              (type-error tvrcfae "matching variant names"))
        )]
    )
  )
)

(define (get-type name-to-find env)
 (type-case TypeEnv env
   [mtEnv () (error 'get-type "free variable, so no type")]
   [aBind (name ty rest)
          (if (symbol=? name-to-find name)
              ty
              (get-type name-to-find rest))]
   [tBind (name var1-name var1-ty var2-name var2-ty rest)
          (get-type name-to-find rest)]
 )
)

(define (find-type-id name-to-find env)
 (type-case TypeEnv env
   [mtEnv () (error 'get-type "free variable, so no type")]
   [aBind (name ty rest)
          (find-type-id name-to-find rest)]
   [tBind (name var1-name var1-ty var2-name var2-ty rest)
          (if (symbol=? name-to-find name)
              env
              (find-type-id name-to-find rest))]
 )
)

(define (validtype ty env)
  (type-case Type ty
    [numT   ()    (mtEnv)]
    [boolT  ()    (mtEnv)]
    [arrowT (a b) (begin (validtype a env)
                         (validtype b env))]
    [idT    (id)  (find-type-id id env)]
  )
)

(define (parse-type te)
  (type-case TE te
    [numTE   ()    (numT)]
    [boolTE  ()    (boolT)]
    [arrowTE (a b) (arrowT (parse-type a)
                           (parse-type b))]
    [idTE    (name)(idT name)]
  )
)

(define (type-error tvrcfae msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string tvrcfae)
                      (string-append " not " msg)))
   )
)

;;
;; -----------------------------------------------------------
;;                       Evaluation
;; -----------------------------------------------------------

(define eval : (TVRCFAE -> TVRCFAE-Value)
  (lambda (tvrcfae)
    (begin
      (try (typecheck tvrcfae (mtEnv))
           (lambda () (error 'typecheck "type-error")))
      (interp tvrcfae (mtSub))
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

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (numTE) (numTE))
                         (app (id 'apple) (num 10)))
              (mtSub))
      (variantV false (numV 10)))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (numTE) (numTE))
                         (cases 'fruit (app (id 'apple) (num 10))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (num 10))))
              (mtSub))
      (numV 11))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (numTE) (numTE))
                         (cases 'fruit (app (id 'banana) (fun 'x (numTE) (sub (id 'x) (num 1))))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (num 10))))
              (mtSub))
      (numV 9))

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


(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (numTE) (numTE))
                            (app (id 'apple) (num 10)))
                 (mtEnv))
      (idT 'fruit))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (numTE) (numTE))
                            (fun 'x (idTE 'fruit) (num 10)))
                 (mtEnv))
      (arrowT (idT 'fruit) (numT)))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (numTE) (numTE))
                            (cases 'fruit (app (id 'apple) (num 10))
                              'apple 'y (add (id 'y) (num 1))
                              'banana 'x (app (id 'x) (num 10))))
                 (mtEnv))
      (numT))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (numTE) (numTE))
                            (cases 'fruit (app (id 'banana) (fun 'x (numTE) (sub (id 'x) (num 1))))
                              'apple 'y (add (id 'y) (num 1))
                              'banana 'x (app (id 'x) (num 10))))
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

(test/exn (typecheck (fun 'x (idTE 'fruit) (id 'x))
                     (mtEnv))
          "no type")

;; cases in wrong order:
(test/exn (typecheck (with-type 'fruit 'apple (numTE)
                                'banana (arrowTE (numTE) (numTE))
                                (cases 'fruit (app (id 'apple) (num 10))
                                  'banana 'y (add (id 'y) (num 1))
                                  'apple 'x (app (id 'x) (num 10))))
                     (mtEnv))
          "no type")

(test/exn (typecheck (with-type 'fruit 'apple (numTE)
                                'banana (arrowTE (numTE) (numTE))
                                (cases 'fruit (app (id 'apple) (num 10))
                                  'apple 'y (app (id 'y) (num 1))
                                  'banana 'x (app (id 'x) (num 10))))
                     (mtEnv))
          "no type")

(test/exn (typecheck (with-type 'fruit 'apple (numTE)
                                'banana (arrowTE (numTE) (numTE))
                                (cases 'fruit (app (id 'apple) (num 10))
                                  'apple 'y (add (id 'y) (num 1))
                                  'banana 'x (add (id 'x) (num 10))))
                     (mtEnv))
          "no type")

(test/exn (typecheck (with-type 'fruit 'apple (numTE)
                                'banana (arrowTE (numTE) (numTE))
                                (app (id 'apple) (fun 'x (numTE) (id 'x))))
                     (mtEnv))
          "no type")

(test/exn (typecheck (with-type 'fruit 'apple (numTE)
                                'banana (arrowTE (numTE) (numTE))
                                (app (id 'banana) (num 10)))
                     (mtEnv))
          "no type")