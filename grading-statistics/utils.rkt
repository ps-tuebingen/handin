#lang racket/base

(provide (all-defined-out))
(require math/matrix)

; Some "missing library" utilities
; ================================

; Computes the regression polynomial of grade n (no math library available for it)
(define ((regression-polynomial xs ys n) x)
    (let* ([vandermonde (vandermonde-matrix xs (+ n 1))]
           [vandermonde-t (matrix-transpose vandermonde)]
           [coeffs (matrix->vector (matrix-solve (matrix* vandermonde-t vandermonde)
                                                 (matrix* vandermonde-t (->col-matrix ys))))])
      (for/sum ([c coeffs]
                [i (in-naturals)])
        (* c (expt x i)))))
