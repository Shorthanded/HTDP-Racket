(require 2htdp/image)
;; tuition-graph-starter.rkt  (just the problem statements)

; 
; PROBLEM:
; 
; Eva is trying to decide where to go to university. One important factor for her is 
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design, 
; so she decides to design a program that will help her visualize the costs at 
; different schools. She decides to start simply, knowing she can revise her design
; later.
; 
; The information she has so far is the names of some schools as well as their 
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
; 
; 
;         .
;         
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.
; 


(define BAR-COLOR "lightblue")
(define BAR-WIDTH 30)

(define Y-SCALE 1/200)

(define TEXT-COLOR "black")
(define TEXT-SIZE 20)

;; Data Definitions

(define-struct school (name tuition))
;; School is (make-school String Number)
;; interp a school's name and tuition in dollars
(define UofO     (make-school "University of Ottawa" 10000))
(define Carleton (make-school"Carleton University" 9000))

(define (fn-for-school s)
  (... (school-name    s)   ; name of the school
       (school-tuition s)))  ; school tuition cost in dollars
  
;; Template rules used:
;;  - compund data: (make-school String Number)

;; ListOfSchool is one of:
;;  - empty
;;  - (cons School ListOfSchool)
;; interp a list of schools
(define LIST1 empty)
(define LIST2 (cons UofO empty))
(define LIST3 (cons Carleton (cons UofO empty)))
#;
(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (fn-for-school (first los))
          (fn-for-los(rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomc distinct: empty
;;  - cmopund: (cons School ListOfSchool)
;;  - reference rule: (first los) is Student
;;  - self-reference: (rest los) is ListOfSchool

;; Functions

;; ListOfSchools -> Image
;; produce bar chart showing names and relative tuitions of consumed schools
(check-expect (chart empty) (square 0 "solid" "white"))
(check-expect (chart (cons (make-school "S1" 8000) empty))
              (beside/align "bottom" (overlay/align "center" "bottom" 
                                     (rotate 90 (text "S1" TEXT-SIZE TEXT-COLOR))
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                                     (square 0 "solid" "white")))
(check-expect (chart (cons (make-school "S1" 8000) (cons (make-school "S2" 12000) empty)))
              (beside/align "bottom" 
                      (overlay/align "center" "bottom"
                                     (rotate 90 (text "S1" TEXT-SIZE TEXT-COLOR))
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR))
                      (overlay/align "center" "bottom" 
                                     (rotate 90 (text "S2" TEXT-SIZE TEXT-COLOR))
                                     (rectangle BAR-WIDTH (* 12000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 12000 Y-SCALE) "solid" BAR-COLOR))
                                     (square 0 "solid" "white")))

;(define (chart los) (square 0 "solid" "white")) ; stub

(define (chart los)
  (cond
    [(empty? los) (square 0 "solid" "white")]
    [else
     (beside/align "bottom" 
                   (make-bar (first los))
                   (chart(rest los)))]))

;; School -> Image
;; Produce a bar for a single school in the bar chart
;; !!!
(check-expect (make-bar (make-school "S1" 8000))
              (overlay/align "center" "bottom" 
                                     (rotate 90 (text "S1" TEXT-SIZE TEXT-COLOR))
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "outline" "black")
                                     (rectangle BAR-WIDTH (* 8000 Y-SCALE) "solid" BAR-COLOR)))


;(define (draw-bar s)(square 0 "solid" "white"))

(define (make-bar s)
  (overlay/align "center" "bottom"
                 (rotate 90 (text (school-name s) TEXT-SIZE TEXT-COLOR))
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "outline" "black")
                 (rectangle BAR-WIDTH (* (school-tuition s) Y-SCALE) "solid" BAR-COLOR)))

