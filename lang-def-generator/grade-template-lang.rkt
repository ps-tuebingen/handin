#lang racket/base

(require (for-syntax racket))
(require (for-syntax racket/base))

(provide (rename-out [gf-def-module-begin #%module-begin]))

(provide #%app)
(provide #%datum)
(provide #%top-interaction) ; Allow running REPL from `grade-template.rktd`

; Syntax helper for checking exercise entry
(define-for-syntax (check-exercise descr maxp)
  (lambda (stx i)
    (let ([tdescr (list-ref descr i)]
          [tmaxp (list-ref maxp i)])
      (syntax-case stx ()
        [(d p) (if (string? (syntax->datum #'d))
                   (if (exact-nonnegative-integer? (syntax->datum #'p))
                       (if (string=? (syntax->datum #'d) tdescr)
                           (if (<= (syntax->datum #'p) tmaxp)
                               #t
                               (raise-syntax-error 'exercise-entry "too many points on exercise"))
                           (raise-syntax-error 'exercise-entry "description doesn't match template"))
                       (raise-syntax-error 'exercise-entry "points not nonnegative integer"))
                   (raise-syntax-error 'exercise-entry "description not string"))]))))

(define-for-syntax (grading-finished-entry? stx)
  (syntax-case stx ()
    [(id _) (symbol=? (syntax->datum #'id) 'grading-finished)]))

; List-of Syntax -> List-of String
; (syntax of the form (string-literal symbol integer-literal))
(define-for-syntax (description stx)
  (syntax-case stx ()
    [(d a p) (syntax->datum #'d)]))

; List-of Syntax -> List-of Integer
; (syntax of the form (string-literal symbol integer-literal))
(define-for-syntax (maxpoints stx)
  (syntax-case stx ()
    [(d a p) (syntax->datum #'p)]))

; Macro generating macro checking language
; ----------------------------------------

(define-syntax (gf-def-module-begin stx)
  (syntax-case stx ()
    [(_ (tg-f texrcs ...))
     (let* ([stxlist (syntax->list #'(texrcs ...))]
            [descr (map description stxlist)]
            [maxp (map maxpoints stxlist)])
           #`(#%module-begin
              (provide (rename-out [gf-module-begin #%module-begin]))
              (provide grading-finished)
              (provide #%datum)
              (provide #%app)
              (provide #%top-interaction) ; Allow running REPL from users of `grade-template.rktd`, that is, `grade.rktd` files.

              (define-syntax (gf-module-begin stx)
                (syntax-case stx ()
                  [(_ (g-f exrcs (... ...))) (if (grading-finished-entry? #'g-f)
                                                 (if (= (length (syntax->datum #'(exrcs (... ...)))) #,(length maxp))
                                                     (when (andmap (check-exercise (list #,@descr) (list #,@maxp)) (syntax->list #'(exrcs (... ...))) (range #,(length maxp)))
                                                       #'(#%module-begin (g-f #t)))
                                                     (raise-syntax-error 'top-level "wrong number of exercise entries"))
                                                 (raise-syntax-error 'top-level "first item not 'grading finished' entry"))]))

              (define-syntax (grading-finished stx)
                (syntax-case stx ()
                  [(_ bool) (if (boolean? (syntax->datum #'bool))
                                #'void
                                (raise-syntax-error 'grading-finished-entry "not a boolean"))]))))]))
