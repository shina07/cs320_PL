#lang plai
;; Simulating a Web Server

;; HTTP protocol is like calling a function
(define total 0)

(define (a)
  `(("Current value:" ,total)
    "Call a2 to add 2"
    "Call a3 to add 3"))

(define (a2)
  (set! total (+ total 2))
  (a))

(define (a3)
  (set! total (+ total 3))
  (a))

;; > (a)
;; '(("Current value:" 0) "Call a2 to add 2" "Call a3 to add 3")
;; > (a2)
;; '(("Current value:" 2) "Call a2 to add 2" "Call a3 to add 3")
;; > (a3)
;; '(("Current value:" 5) "Call a2 to add 2" "Call a3 to add 3")




;; Stateless variant is functions with arguments

(define (b)
  (do-b 0))

(define (do-b total)
  `(("Current value:" ,total)
    "Call b2 with " ,total " to add 2"
    "Call b3 with " ,total " to add 3"))

(define (b2 total)
  (do-b (+ total 2)))

(define (b3 total)
  (do-b (+ total 3)))



;; For complex data, use remember and lookup to make a simple key:

(define (c)
  (do-c "*"))

(define (do-c total)
  (local [(define key (remember total))]
    `(("Current value:" ,total)
      "Call c2 with " ,key " to append \"hello\""
      "Call c3 with " ,key " to append \"goodbye\"")))

(define (c2 key)
  (do-c (string-append (lookup key) " hello")))

(define (c3 key)
  (do-c (string-append (lookup key) " goodbye")))

(define table empty)

(define (remember v)
  (local [(define n (length table))]
    (begin
      (set! table (append table (list v)))
      n)))

(define (lookup key)
  (list-ref table key))



;; But normally we write code more like this:

;; Directive Interactive Programs
(define (d)
  (do-d 0))

(define (do-d total)
  (begin
    (printf "Total is ~a\nAdd 2 next? \n" total)
    (do-d (+ total
             (if (read) 2 3)))))


;; Or like this
(define (e)
  (do-e 0))

(define (num-read prompt)
  (begin
    (printf "~a\n" prompt)
    (read)))

(define (do-e total)
  (do-e (+ (num-read
            (format "Total is ~a\n Next number... \n" total))
           total)))


;; We'd like to have a "web-read"...

;;Can we makk this work?
(define (f)
  (do-f))

(define (web-read/k prompt cont)
  (local [(define key (remember cont))]
    `(,prompt "To continue, call resume/k with " ,key " and value")))

(define (resume/k key val)
  (local [(define cont (lookup key))]
    (cont val)))

(define (do-f total)
  (web-read/k
   (format "Total is ~a\nNext number...\n" total)
   (lambda (val)
     (do-f (+ total val)))))



