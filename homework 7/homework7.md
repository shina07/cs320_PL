# Homework #7



- #####Improving Assignments

	Start with the BFAE interpreter. In the starting program, the representation of the store grows every time that a box's content is modified with setbox. Change the implementation of setbox so that the old value of the box is dropped (i.e., replaced with the new value) instead of merely hidden by the outside-in search order of store-lookup.
    
    Example:
    
        (test (interp (parse '{{fun {b}
                                  {seqn
                                   {setbox b 2}
                                   {openbox b}}}
                                 {newbox 1}})
                        (mtSub)
                        (mtSto))
                (v*s (numV 2)
                     (aSto 1 (numV 2) (mtSto))))
             



- #####Improving Sequences

	Generalize seqn to allow one or more sub-expressions, instead of exactly two sub-expressions:

        <BFAE>  ::= ...
                 | {seqn <BFAE> <BFAE>*}
    
    Example:

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

- #####Records
 
	Add rec and get forms for records as in Homework #6, and also add set form that modifies the value of a record field:

        <BFAE>  ::= ...
                 | {rec {<id> <BFAE>}*}
                 | {get <BFAE> <id>}
                 | {set <BFAE> <id> <BFAE>}
                 
	A rec form allocates a new record. A get form accesses from the record produced by the sub-expression the value of the field named by the identifier. A set form changes within the record produced by the first sub-expression the value of the field named by the identifer; the value of the second sub-expression determines the field's new value, and that value is also the result of the set expression. Evaluate the first sub-expression before evaluating the second sub-expression. Note that changes within the record using the field name that does not exist in the record print error messages “no such field”. Therefore, a rec is a mutable data structure as same as a box. For example,
    
    	(interp-expr (parse '{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}))
        
	the above expression returns 5 as a value. You can implement a record in various ways, but using store is a possible option. Also extend parse and define the usual interp-expr, which takes an expression and interprets it with an empty initial environment and empty initial store, and returns either a number, 'func, 'box, 'record (without returning the store). Also, we will test your code for a record only with interp-expr.
    
    Examples:
    
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