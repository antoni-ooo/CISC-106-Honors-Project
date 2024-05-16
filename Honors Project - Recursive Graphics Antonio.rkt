;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Honors Project - Recursive Graphics Antonio 5550206043|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket)
(require 2htdp/image)
(require racket/local)

; ///////////////////////////////////////////////////////////////////////////////////////////////

; PURPOSE:
; To create a function that returns a list whose sole element is a random color

; SIGNATURE:
; -> color

; CODE
(define (rand-color functionality)
  (color (random 256) (random 256) (random 256))
)

; PURPOSE:
; To build a vector whose elements consist of random colors

; SIGNATURE:
; Number (amount of desired random colors) -> Vector with arg amount of random colors

; CODE:
(define (vector-of-colors quantity)
  (build-vector quantity rand-color)
)

; PURPOSE:
; To convert a vector of random colors to a list of random colors

; SIGNATURE:
; vector -> list

; CODE:
(define (color-list quantity)
  (vector->list (vector-of-colors quantity))
)

; ///////////////////////////////////////////////////////////////////////////////////////////////
; SIERPINSKI CARPET /////////////////////////////////////////////////////////////////////////////

; PURPOSE:
; to implement a randomly-colored Sierpinski Carpet producing recursive function

; SIGNATURE:
; List -> Image

; EXAMPLES:
(define test-color-list-1 (color-list 1))
(define test-color-list-2 (color-list 2))
(check-expect (sierpinski-carpet test-color-list-1) (square 4 "solid" (first test-color-list-1)))
(check-expect (sierpinski-carpet test-color-list-2) (above (beside (square 4 "solid" (first (rest test-color-list-2)))
                                                                   (square 4 "solid" (first (rest test-color-list-2)))
                                                                   (square 4 "solid" (first (rest test-color-list-2))))
                                                           (beside (square 4 "solid" (first (rest test-color-list-2)))
                                                                   (square 4 "solid" (first test-color-list-2))
                                                                   (square 4 "solid" (first (rest test-color-list-2))))
                                                           (beside (square 4 "solid" (first (rest test-color-list-2)))
                                                                   (square 4 "solid" (first (rest test-color-list-2)))
                                                                   (square 4 "solid" (first (rest test-color-list-2))))))                                                                   

; CODE:
(define (sierpinski-carpet rand-color-list)                       ; accepts a LIST! (color_list ...) must be the argument here!
  (define base-square (square 4 "solid" (first rand-color-list)))
  (cond
    ((empty? (rest rand-color-list)) base-square)
    [else
       (define c (sierpinski-carpet (rest rand-color-list)))
       (define i (square (image-width c) "solid" (first rand-color-list)))
     (above (beside c c c)
            (beside c i c)
            (beside c c c)
     )
    ]
  )
)

(define tapestry (sierpinski-carpet (color-list 5)))

tapestry

; ///////////////////////////////////////////////////////////////////////////////////////////////
; TREE //////////////////////////////////////////////////////////////////////////////////////////

; EXAMPLES:
(check-expect (make-tree 1 30) (rectangle 1 length "solid" "dark green"))
(check-expect (make-tree 2 30) (above (beside (rotate 30 (rectangle 1 (/ 30 1.74) "solid" "dark green")) (rotate -30 (rectangle 1 (/ 30 1.74) "solid" "dark green")))(rectangle 1 (* length 1.74 1.74 1.74 1.74) "solid" "brown")))

; CODE:
(define (make-tree num length)
  (define factor 1.74)
  (define new-length (/ length factor))
  (define leaf (rectangle 1 length "solid" "dark green"))
  (cond
    ((= num 1) leaf)
    (else
     (define next-level-tree (make-tree (- num 1) new-length))
     (above
      (beside
       (rotate 30 next-level-tree)
       (rotate -30 next-level-tree)
      )
     (rectangle 1 (* length factor factor factor factor) "solid" "brown")
     )
    )
  )
)

(define length 30)  
(make-tree 7 length)
(make-tree 8 length)

; ///////////////////////////////////////////////////////////////////////////////////////////////
; SIERPINSKI TRIANGLE ///////////////////////////////////////////////////////////////////////////

; EXAMPLES:
(check-expect (sierpinski-triangle test-color-list-1) (triangle 40 "solid" (first test-color-list-1)))

(check-expect (sierpinski-triangle test-color-list-2) (above
                                      (triangle 40 "solid" (first (rest test-color-list-2)))
                                            (beside
                                             (triangle 40 "solid" (first (rest test-color-list-2)))
                                             (triangle 40 "solid" (first (rest test-color-list-2))))))


; CODE:
(define (sierpinski-triangle color-lst)
  (define base-tri (triangle 40 "solid" (first color-lst)))                           
  (cond
    ((empty? (rest color-lst)) base-tri)
    (else    
     (define recursive-call (sierpinski-triangle (rest color-lst)))
     (define recursive-triangle
       (above recursive-call
              (beside recursive-call recursive-call)
       )
     )
     recursive-triangle
    )
  )
)

(define pyramid-scheme (sierpinski-triangle (color-list 5)))

pyramid-scheme

; ///////////////////////////////////////////////////////////////////////////////////////////////
; KOCH CURVE ////////////////////////////////////////////////////////////////////////////////////

; EXAMPLES:
(check-expect (koch-curve test-color-list-1) (square 1 "solid" (first test-color-list-1)))
(check-expect (koch-curve test-color-list-2) (beside/align "bottom"
                                                           (square 1 "solid" (first (rest test-color-list-2)))
                                                           (rotate 60 (square 1 "solid" (first (rest test-color-list-2))))
                                                           (rotate -60 (square 1 "solid" (first (rest test-color-list-2))))
                                                           (square 1 "solid" (first (rest test-color-list-2)))))

(define (koch-curve color-lst)
  (define base-square (square 1 "solid" (first color-lst)))
  (cond
    ((empty? (rest color-lst)) base-square)
    (else
     (define smaller (koch-curve (rest color-lst)))
     (beside/align "bottom"
                   smaller
                   (rotate 60 smaller)
                   (rotate -60 smaller)
                   smaller
     )
    )
  )
)

(define curve (koch-curve (color-list 6)))

curve

(koch-curve (color-list 5))

; SNOWFLAKE!

(define koch-snowflake
  (above
   (beside
    (rotate 60 (koch-curve (color-list 6)))
    (rotate -60 (koch-curve (color-list 6)))
    )
   (flip-vertical (koch-curve (color-list 6)))
  )
)

koch-snowflake