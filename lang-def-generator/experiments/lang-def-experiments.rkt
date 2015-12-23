#lang racket

(require (for-syntax racket))

(provide (rename-out [gf-module-begin #%module-begin]))

(provide #%datum)

(provide #%app)

(provide gf)

(provide grading-finished)

(define-for-syntax (grading-finished-entry? stx)
  (syntax-case stx ()
    [(id _) (symbol=? (syntax->datum #'id) 'grading-finished)]))

(define-syntax (gf stx)
  (syntax-case stx ()
    [(_ (g-f exrcs ...)) (if (grading-finished-entry? #'g-f)
                           #'(list g-f (exercise exrcs) ...)
                           (raise-syntax-error 'top-level "first item not 'grading finished' entry"))]))

(define-syntax (grading-finished stx)
  (syntax-case stx ()
    [(_ bool) (if (boolean? (syntax->datum #'bool))
                  #'(list bool)
                  (raise-syntax-error 'grading-finished-entry "not a boolean"))]))

(define-syntax (exercise stx)
  (syntax-case stx ()
    [(_ [descr p]) (if (string? (syntax->datum #'descr))
                        (if (integer? (syntax->datum #'p))
                            #'(list descr p)
                            (raise-syntax-error 'exercise-entry "points not integer"))
                        (raise-syntax-error 'exercise-entry "description not string"))]
    [(_ [_]) (raise-syntax-error 'exercise-entry "exercise entry consists of two parts")]
    [(_ []) (raise-syntax-error 'exercise-entry "exercise entry consists of two parts")]))

; credit: http://juanibiapina.com/articles/2014-10-03-writing-a-language/
; (actually not sure if necessary, could also use #%module-begin directly)
(define-syntax (gf-module-begin stx)
  (datum->syntax
     stx
     (cons (quote-syntax #%module-begin)
           (map (lambda (e)
                  (list (quote-syntax gf)
                        e))
                (cdr (syntax-e stx))))
     stx
     stx))
