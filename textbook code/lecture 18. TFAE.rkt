#lang plai-typed

(define-type TFAE
  [num (n : number)]
  [add (lhs : TFAE) (rhs : TFAE)]
  [sub (lhs : TFAE) (rhs : TFAE)]
  [id  (name : symbol)]
  [fun (param : symbol) (type : TE) (body : TFAE)]
  [app (fun-expr : TFAE) (arg-expr : TFAE)]
)