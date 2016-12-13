#lang racket
(require 2htdp/image
         threading)

(define dot (rectangle 1 1 'solid 'white))
(define scene (rectangle 800 800 'solid 'black))

;; Complex Integer -> (Listof Complex)
;; Takes a starting Complex number and a max depth, returns a mandelbrot
;; escape list (possibly empty) of points 
(define (mandelbrot-list x d)
  (define (iter result count)
    (cond
      [(> (magnitude (first result)) 2.0)
       (rest result)]
      [(<= count 1)
       '()]
      [else (iter (cons (+ x (sqr (first result))) result)
                  (sub1 count))]))
  (iter (cons x '()) d))

;; Integer Integer -> (Listof (Listof Complex))
;; returns n random (Listof Complex) mandelbrot escape points with
;; max depth of d, each Complex number between -2 and 2
(define (mandelbrot-points n d)
  (map (λ (x) (mandelbrot-list (make-rectangular (- (* 4 (random)) 2)
                                                 (- (* 4 (random)) 2))
                               d))
       (range n)))

(define locations
(~> (mandelbrot-points 35000 100)
    (apply append _)
    (map (λ (x) (* 200 (+ x 2.0+2.0i))) _)
    (map (λ (x) (list (exact-floor (real-part x))
                      (exact-floor (imag-part x)))) _)))

(for-each (λ (pt)
            (set! scene (place-image dot
                                     (first pt) (second pt)
                                     scene)))
          locations)
scene

