#lang racket
(require threading
         racket/flonum
         images/flomap)

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

(define locations-flvector
  (make-flvector (* 800 800) 0.0))

(for-each (λ (pt)
            (let ([pos (+ (first pt) (* 800 (second pt)))])
              (flvector-set! locations-flvector pos
                           (add1 (flvector-ref locations-flvector pos)))))
          locations)

(define budda (flomap locations-flvector 1 800 800))
(~> budda
    flomap-normalize
    (flomap-rotate _ (* -1/2 pi))
    (flomap-ct-crop _ 500 600)
    flomap->bitmap)

