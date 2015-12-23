#lang racket

(require (for-syntax racket))

(provide (rename-out [gf-module-begin #%module-begin]))

(provide #%datum)

(provide #%app)

(provide gf)

(provide grading-finished)

(define-for-syntax MAX-POINTS (list 25 30 15 10))

(define-for-syntax DESCRIPTIONS (list "Ex. 1" "Ex. 2" "Ex. 3" "Ex. 4"))

(define-for-syntax (grading-finished-entry? stx)
  (syntax-case stx ()
    [(id _) (symbol=? (syntax->datum #'id) 'grading-finished)]))

(define-for-syntax (enrich-exercise stx i)
  (syntax-case stx ()
    [(d p) #`(exercise [d p] [#,(list-ref DESCRIPTIONS i) #,(list-ref MAX-POINTS i)])]))

(define-syntax (gf stx)
  (syntax-case stx ()
    [(_ (g-f exrcs ...)) (if (grading-finished-entry? #'g-f)
                             (if (= (length (syntax->datum #'(exrcs ...))) (length MAX-POINTS))
                                 #`(cons g-f
                                         #,(map enrich-exercise (syntax->list #'(exrcs ...)) (range (length MAX-POINTS))))
                                 (raise-syntax-error 'top-level "wrong number of exercise entries"))
                             (raise-syntax-error 'top-level "first item not 'grading finished' entry"))]))

(define-syntax (grading-finished stx)
  (syntax-case stx ()
    [(_ bool) (if (boolean? (syntax->datum #'bool))
                  #'(list bool)
                  (raise-syntax-error 'grading-finished-entry "not a boolean"))]))

(define-syntax (exercise stx)
  (syntax-case stx ()
    [(_ [descr p] [tdescr maxp]) (if (string? (syntax->datum #'descr))
                                     (if (exact-nonnegative-integer? (syntax->datum #'p))
                                         (if (string=? (syntax->datum #'descr) (syntax->datum #'tdescr))
                                             (if (<= (syntax->datum #'p) (syntax->datum #'maxp))
                                                 #'(list descr p)
                                                 (raise-syntax-error 'exercise-entry "too many points on exercise"))
                                             (raise-syntax-error 'exercise-entry "description doesn't match template"))
                                         (raise-syntax-error 'exercise-entry "points not nonnegative integer"))
                                     (raise-syntax-error 'exercise-entry "description not string"))]
    [(_ [_] _) (raise-syntax-error 'exercise-entry "exercise entry consists of two parts")]
    [(_ [] _) (raise-syntax-error 'exercise-entry "exercise entry consists of two parts")]))

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
