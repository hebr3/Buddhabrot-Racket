;;;;;;;;;1;;;;;;;;;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7;;
#lang racket
(require threading
         math/flonum
         images/flomap
         racket/draw)

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


;;;;;;;;;;;;;;;
(define locations-blue
(time
(~> (mandelbrot-points 1500000 64)
    (apply append _)
    (map (λ (x) (* 400 (+ x 2.0+2.0i))) _)
    (map (λ (x) (list (exact-floor (real-part x))
                      (exact-floor (imag-part x)))) _))))
'location-blue
(define locations-flvector-blue
  (make-flvector (* 1600 1600) 0.0))

(for-each (λ (pt)
            (let ([pos (+ (first pt) (* 1600 (second pt)))])
              (flvector-set! locations-flvector-blue pos
                           (add1 (flvector-ref locations-flvector-blue pos)))))
          locations-blue)

(define budda-blue (flomap locations-flvector-blue 1 1600 1600))

(define blue-fm
(time
(~> budda-blue
    flomap-normalize
    flomap-values
    flvector->list
    (map (λ (x) (list x 0.0 0.0 x)) _)
    (apply append _)
    list->flvector
    (flomap _ 4 1600 1600)
    (flomap-rotate _ (* -1/2 pi)))))
'blue-fm
;;;;;;;;;;;
(define locations-teal (time
(~> (mandelbrot-points 1500000 256)
    (apply append _)
    (map (λ (x) (* 400 (+ x 2.0+2.0i))) _)
    (map (λ (x) (list (exact-floor (real-part x))
                      (exact-floor (imag-part x)))) _))))
'locations-teal
(define locations-flvector-teal
  (make-flvector (* 1600 1600) 0.0))

(for-each (λ (pt)
            (let ([pos (+ (first pt) (* 1600 (second pt)))])
              (flvector-set! locations-flvector-teal pos
                           (add1 (flvector-ref locations-flvector-teal pos)))))
          locations-teal)

(define budda-teal (flomap locations-flvector-teal 1 1600 1600))

(define teal-fm
(time
(~> budda-teal
    flomap-normalize
    flomap-values
    flvector->list
    (map (λ (x) (list x 0.0 x x)) _)
    (apply append _)
    list->flvector
    (flomap _ 4 1600 1600)
    (flomap-rotate _ (* -1/2 pi)))))
'teal-fm
;;;;;;;;;;;
(define locations-yellow (time
(~> (mandelbrot-points 1500000 1024)
    (apply append _)
    (map (λ (x) (* 400 (+ x 2.0+2.0i))) _)
    (map (λ (x) (list (exact-floor (real-part x))
                      (exact-floor (imag-part x)))) _))))
'locations-yellow
(define locations-flvector-yellow
  (make-flvector (* 1600 1600) 0.0))

(for-each (λ (pt)
            (let ([pos (+ (first pt) (* 1600 (second pt)))])
              (flvector-set! locations-flvector-yellow pos
                           (add1 (flvector-ref locations-flvector-yellow pos)))))
          locations-yellow)

(define budda-yellow (flomap locations-flvector-yellow 1 1600 1600))

(define yellow-fm
(time
(~> budda-yellow
    flomap-normalize
    flomap-values
    flvector->list
    (map (λ (x) (list x x x 0.0)) _)
    (apply append _)
    list->flvector
    (flomap _ 4 1600 1600)
    (flomap-rotate _ (* -1/2 pi)))))
'yellow-fm
;;;;;;;;;;;
(define locations-red (time
(~> (mandelbrot-points 1500000 5120)
    (apply append _)
    (map (λ (x) (* 400 (+ x 2.0+2.0i))) _)
    (map (λ (x) (list (exact-floor (real-part x))
                      (exact-floor (imag-part x)))) _))))
'loc-red
(define locations-flvector-red
  (make-flvector (* 1600 1600) 0.0))

(for-each (λ (pt)
            (let ([pos (+ (first pt) (* 1600 (second pt)))])
              (flvector-set! locations-flvector-red pos
                           (add1 (flvector-ref locations-flvector-red pos)))))
          locations-red)

(define budda-red (flomap locations-flvector-red 1 1600 1600))

(define red-fm
(time
(~> budda-red
    flomap-normalize
    flomap-values
    flvector->list
    (map (λ (x) (list x x 0.0 0.0)) _)
    (apply append _)
    list->flvector
    (flomap _ 4 1600 1600)
    (flomap-rotate _ (* -1/2 pi)))))
'red-fm

(define black-fm (make-flomap* 1600 1600 #(1.0 0.0 0.0 0.0)))

(time
(~> black-fm
    (fm+ blue-fm)
    (fm+ teal-fm)
    (fm+ yellow-fm)
    (fm+ red-fm)
    (flomap-scale _ 1/2)
    flomap->bitmap
    (send _ save-file "Buddabrot.png" 'png)))
