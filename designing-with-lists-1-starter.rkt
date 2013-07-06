
;; designing-with-lists-1-starter.rkt

; 
; PROBLEM:
; 
; You've been asked to design a program having to do with all the owls
; in the owlery.
; 
; (A) Design a data definition to represent the weights of all the owls. 
;     For this problem call it ListOfNumber.
; (B) Design a function that consumes the weights of owls and produces
;     the total weight of all the owls.
; (C) Design a function that consumes the weights of owls and produces
;     the total number of owls.
;     


;; Data Definition

;; ListOfNumber is one of:
;; - empty
;; (cons Number ListOfNumber)
;; interp. each number in the list is an owl weight in ounces
(define LON1 empty)
(define LON2 (cons 34 empty))
(define LON3 (cons 45 (cons 34 empty)))
#;
(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else
     (... (first lon)
          (fn-for-lon(rest lon)))]))
  
;; Template rules used empty
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber

;; Functions:

;; ListOfNumber -> Number
;; produce total weight of owls in consumed list
(check-expect (sum empty) 0)
(check-expect (sum (cons 60 empty)) (+ 60 0))
(check-expect (sum (cons 60 (cons 42 empty))) (+ 60 (+ 42 0)))

;(define (sum lon) 0) ;stub

(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else
     (+ (first lon)
          (sum (rest lon)))]))

;; ListOfNumber -> Natural
;; Produce total number of weights in consumed list
(check-expect (weights empty) 0)
(check-expect (weights (cons 60 empty)) (+ 1 0))
(check-expect (weights (cons 60 (cons 42 empty))) (+ 1 (+ 1 0)))

;(define (weights lon) 0) ; stub

(define (weights lon)
  (cond
    [(empty? lon) 0]
    [else
     (+ 1 (weights(rest lon)))]))

