#lang racket/base

(require (for-syntax racket))
(require (for-syntax racket/base))

(provide (rename-out [gf-def-module-begin #%module-begin]))

(provide #%app)
(provide #%datum)
(provide #%top-interaction) ; Allow running REPL from `grade-template.rktd`

(define-for-syntax (check-synobj-satisfies pred synob source errstr)
  (if (pred (syntax->datum synob))
      #true
      (raise-syntax-error source errstr synob)))

; Syntax helper for checking exercise entry
(define-for-syntax (check-exercise descr maxp)
  (lambda (stx i)
    (let ([tdescr (list-ref descr i)]
          [tmaxp (list-ref maxp i)])
      (syntax-case stx ()
        [(d p)
         (and
          (check-synobj-satisfies string? #'d 'exercise-entry "description not string")
          (check-synobj-satisfies exact-integer? #'p 'exercise-entry "points not integer")
          (check-synobj-satisfies exact-nonnegative-integer? #'p 'exercise-entry "points not >= 0")
          (check-synobj-satisfies (λ (descr-tested) (string=? descr-tested tdescr))
                                  #'d
                                  'exercise-entry
                                  "description doesn't match template")

          (check-synobj-satisfies (λ (points) (<= points tmaxp))
                                  #'p
                                  'exercise-entry
                                  "too many points on exercise"))]))))

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
                                                     ; What should the source location be?
                                                     (raise-syntax-error 'top-level "wrong number of exercise entries"))
                                                 ; XXX We have a syntax error, but that's imprecise. Where's exactly the problem?
                                                 (raise-syntax-error 'top-level  "first item not a valid 'grading finished' entry" #'g-f))]))

              (define-syntax (grading-finished stx)
                (syntax-case stx ()
                  [(_ bool) (if (boolean? (syntax->datum #'bool))
                                #'void
                                (raise-syntax-error 'grading-finished-entry "not a boolean"))]))))]))
