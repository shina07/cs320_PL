#lang plai

; area-square: number -> number                  [Contract]

; to compute the area of square who has side     [Purpose]
; length of length (in the argument)

; 1. Allow real numbers, even though it is       [Policy]
; stated as "integer" in the question

; 2. Only allow positive number, since "length"
; should be greater than 0

(define (area-square length)
  (cond
    [(not (real? length)) (error "length should be real number")]
    [(not (positive? length)) (error "length should be greater than 0")]
    [else (* length length)]
  )
)

;                                                [Tests]
(test (area-square 4) 16)
(test (area-square 1.2) 1.44)
(test/exn (area-square "a") "length should be real number")
(test/exn (area-square "1") "length should be real number")
(test/exn (area-square 0) "length should be greater than 0")
(test/exn (area-square -1) "length should be greater than 0")


; volume-cuboid: number number number -> number  [Contract]

; to compute the volume of whose width, length   [Purpose]
; and height is given in the argument

; 1. Allow real numbers, even though it is       [Policy]
; stated as "integer" in the question

; 2. Only allow positive number, since "length"
; should be greater than 0

(define (volume-cuboid width length height)
  (cond
    [(not (real? width)) (error "width should be real number")]
    [(not (real? length)) (error "length should be real number")]
    [(not (real? height)) (error "height should be real number")]
    [(not (positive? width)) (error "width should be greater than 0")]
    [(not (positive? length)) (error "length should be greater than 0")]
    [(not (positive? height)) (error "height should be greater than 0")]
    [else (* width length height)]
  )
)

;                                                [Tests]
(test (volume-cuboid 3 4 5) 60)
(test (volume-cuboid 1.3 1.3 1) 1.69)
(test/exn (volume-cuboid "0" 1 1) "width should be real number")
(test/exn (volume-cuboid 1 "0" 1) "length should be real number")
(test/exn (volume-cuboid 1 1 "0") "height should be real number")
(test/exn (volume-cuboid -1 1 1) "width should be greater than 0")
(test/exn (volume-cuboid 1 -1 1) "length should be greater than 0")
(test/exn (volume-cuboid 1 1 -1) "height should be greater than 0")


; is-multiple-of?: number number -> boolean      [Contract]

; to define whether the first argument is a      [Purpose]
; multiple of the second argument

; 1. I decided to consider only integer          [Policy]
; numbers, not all real numbers. Therefore,
; I will reject any float value, even if
; that number can be converted into integer

; "exact-integer?" is even more strict check
; than "real?" so this predicate covers bot non-real
; inputs and non-number inputs.

(define (is-multiple-of? multiple factor)
  (cond
    [(not (exact-integer? multiple)) (error "only consider integers for multiples")]
    [(not (exact-integer? factor)) (error "only consider integers for factors")]
    [(zero? factor) (error "not divisible by 0")]
    [(zero? (modulo multiple factor)) #t]
    [else #f]
  )
)

;                                                [Tests]
(test (is-multiple-of? 10 2) #t)
(test (is-multiple-of? 10 3) #f)
(test/exn (is-multiple-of? 5 0) "not divisible by 0")
(test/exn (is-multiple-of? "10" 2) "only consider integers for multiples")
(test/exn (is-multiple-of? 10 "3") "only consider integers for factors")
(test/exn (is-multiple-of? 10.0 2) "only consider integers for multiples")
(test/exn (is-multiple-of? 10 3.0) "only consider integers for factors")


; factorial: number -> number                    [Contract]

; to calculate the result of factorial           [Purpose]
; operation of a given argument

; 1. I decided to consider only integer          [Policy]
; numbers, not all real numbers. Therefore,
; I will reject any float value, even if
; that number can be converted into integer

; "exact-integer?" is even more strict check
; than "real?" so this predicate covers bot non-real
; inputs and non-number inputs.

(define (factorial n)
  (cond
    [(not (exact-integer? n)) (error "not defined for non integer value")]
    [(negative? n) (error "not defined for negative number")]
    [(equal? n 0) 1]
    [else (* n (factorial (- n 1)))]
  )
)

;                                                [Tests]
(test (factorial 0) 1)
(test (factorial 1) 1)
(test (factorial 4) 24)
(test/exn (factorial "factorial") "not defined for non integer value")
(test/exn (factorial 4.0) "not defined for non integer value")
(test/exn (factorial -1) "not defined for negative number")


; fibonacci: number -> number                    [Contract]

; to calculate n-th fibonacci number             [Purpose]
; for given argument n

; 1. I decided to consider only integer          [Policy]
; numbers, not all real numbers. Therefore,
; I will reject any float value, even if
; that number can be converted into integer

; "exact-integer?" is even more strict check
; than "real?" so this predicate covers bot non-real
; inputs and non-number inputs.

(define (fibonacci n)
    (cond
    [(not (exact-integer? n)) (error "n should be an integer")]
    [(not (positive? n)) (error "n should be a natural number")]
    [(equal? n 1) 1]
    [(equal? n 2) 1]
    [else (+ (fibonacci(- n 1)) (fibonacci(- n 2)))]
  )
)

;                                                [Tests]
(test (fibonacci 1) 1)
(test (fibonacci 2) 1)
(test (fibonacci 5) 5)
(test (fibonacci 8) 21)
(test/exn (fibonacci 0) "n should be a natural number")
(test/exn (fibonacci 3.0) "n should be an integer")
(test/exn (fibonacci "fibonacci") "n should be an integer")



(define-type COURSE
  [CS320 (quiz number?)
         (homework number?)]
  [CS311 (homework number?)]
  [CS330 (projects number?)
         (homework number?)]
 )

; total-assignments: COURSE -> number            [Contract]

; to calculate total number of                   [Purpose]
; assignment, which contains quizzes, homework,
; and projects, for a given course

(define (total-assignments a-course)
  (type-case COURSE a-course
    [CS320 (quiz homework) (+ quiz homework)]
    [CS311 (homework) homework]
    [CS330 (projects homework) (+ projects homework)]
  )
)

;                                                [Tests]
(define course1 (CS320 4 10))
(define course2 (CS311 4))
(define course3 (CS330 5 0))

(test (total-assignments course1) 14)
(test (total-assignments course2) 4)
(test (total-assignments course3) 5)


;course-homework: COURSE -> number               [Contract]

; to calculate total number of homework for      [Purpose]
; a given course. This work as a helper
; function of "total-homework", which is below

(define (course-homework a-course)
  (if (not (COURSE? a-course))
      (error "Should input COURSE type")
      (type-case COURSE a-course
        [CS320 (quiz homework) homework]
        [CS311 (homework) homework]
        [CS330 (projects homework) homework]
      )
  )
)

;                                                [Tests]
(test (course-homework course1) 10)
(test (course-homework course2) 4)
(test (course-homework course3) 0)
(test/exn (course-homework "course") "Should input COURSE type")

; total-homework: ( list of COURSE ) -> number   [Contract]

; to calculate total number of                   [Purpose]
; assignment, which contains quizzes, homework,
; and projects, for a given course

(define (total-homework courses)
  (if (list? courses)
      (if (empty? courses)
          0
          (+ (course-homework (first courses)) (total-homework (rest courses)))
      )
      (error "Should input list")
  )
)

;                                                [Tests]
(define courses (list course1 course2 course3))
(define courses-comb1 (list course1 course2))
(define courses-comb2 (list course2 course3))
(define courses-comb3 (list course3 course1))
(define courses-wrong (list course1 course2 "course"))

(test (total-homework courses) 14)
(test (total-homework courses-comb1) 14)
(test (total-homework courses-comb2) 4)
(test (total-homework courses-comb3) 10)
(test/exn (total-homework "course") "Should input list")
(test/exn (total-homework `(1 2 3)) "Should input COURSE type")
(test/exn (total-homework courses-wrong) "Should input COURSE type")


; my-map: function list -> list                  [Contract]

; to implement a map function in dr.racket,      [Purpose]
; which applies the given function to each
; element of the given list, and produces another
; list with result elements

(define (my-map f l)
  (cond
    [(not (procedure? f)) (error "please input valid procedure")]
    [(not (list? l)) (error "please input valid list")]
    [else (if (empty? l)
              empty
              (cons (f (first l)) (my-map f (rest l)))
          )]
  )
)

;                                                [Tests]

; This test-case is given in the question
(test (my-map (lambda (n) (+ 1 n)) `(1 2 3 4)) `(2 3 4 5))

; This test-case contains the same input for the
; map function in racket documentation. Since it
; is not from my own code, I should leave the
; racket homepage as a source.
; site: https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
; at 2.3.1 Predefined List Loops
(test (my-map sqrt `(1, 4, 9, 16)) `(1, 2, 3, 4))

(test/exn (my-map sqrt "1, 4, 9, 16") "please input valid list")
(test/exn (my-map "sqrt"  `(1, 4, 9, 16)) "please input valid procedure")



; my-filter: predicate list -> list              [Contract]

; to implement a filter function in dr.racket,   [Purpose]
; which applies the given predicate to each
; element of the given list, and produces another
; list which contains only elements that satisfy
; the given predicate

(define (my-filter f l)
  (cond
    [(not (procedure? f)) (error "please input valid procedure")]
    [(not (list? l)) (error "please input valid list")]
    [(if (empty? l)
         empty
         (if (f (first l))
             (cons (first l) (my-filter f (rest l)))
             (my-filter f (rest l))
         )
     )]
  )
)

;                                                [Tests]

; This test-case is given in the question
(test (my-filter (lambda (x) (> x 1)) (cons 3 (cons 1 (cons 2 empty)))) `(3 2))

(test (my-filter (lambda (x) (> x 5)) `(1, 9, 2, 8, 3, 7, 4, 6, 5)) `(9, 8, 7, 6))
(test (my-filter (lambda (x) (zero? (modulo x 3))) `(1, 2, 3, 4, 5, 6, 7, 8, 9)) `(3, 6, 9))
(test/exn (my-filter "test" `(1, 2, 3, 4, 5, 6, 7, 8, 9)) "please input valid procedure")
(test/exn (my-filter (lambda (x) (> x 1)) "this is not a list") "please input valid list")