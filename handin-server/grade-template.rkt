#lang racket/base

(require (for-syntax racket))
(require (for-syntax racket/base))

(provide (rename-out [gf-def-module-begin #%module-begin]))

(provide #%app)
(provide #%datum)
(provide #%top-interaction) ; Allow running REPL from `grade-template.rktd`
(provide grading-finished)

(define-for-syntax (check-satisfies pred synob source errstr)
  (if (pred synob)
      #true
      (raise-syntax-error source errstr synob)))

(define-for-syntax (check-synobj-satisfies pred synob source errstr)
  (check-satisfies (λ (synob) (pred (syntax->datum synob))) synob source errstr))

; Syntax helper for checking exercise entry
(define-for-syntax (check-exercise descr maxp finished-grading)
  (lambda (stx i)
    (let ([tdescr (list-ref descr i)]
          [tmaxp (list-ref maxp i)]
          [point-wrong-type-msg
           (if finished-grading
               "points not integer"
               "points not integer or symbol")])
      (syntax-case stx ()
        [(d p)
         (and
          (check-synobj-satisfies string? #'d 'exercise-entry "description not string")
          (check-synobj-satisfies (λ (descr-tested) (string=? descr-tested tdescr))
                                  #'d
                                  'exercise-entry
                                  "description doesn't match template")
          (or
           ; Allow symbols in unfinished grade files (see
           ; https://github.com/ps-tuebingen/info1-teaching-material/issues/92).
           (and (not finished-grading) (symbol? (syntax->datum #'p)))
           ; Even in unfinished grade files, any numeric grades must be
           ; validated.
           (and
            (check-synobj-satisfies exact-integer? #'p 'exercise-entry point-wrong-type-msg)
            (check-synobj-satisfies exact-nonnegative-integer? #'p 'exercise-entry "points not >= 0")
            (check-synobj-satisfies (λ (points) (<= points tmaxp))
                                    #'p
                                    'exercise-entry
                                    "too many points on exercise"))))]))))

(define-for-syntax (grading-finished-entry? stx)
  (symbol=? (first (syntax->datum stx)) 'grading-finished))

; Check if the passed grading-finished entry marks the file as complete.
(define-for-syntax (grading-finished? stx)
  (syntax-case stx (grading-finished)
    [(grading-finished bool-syn)
     (let ([bool (syntax->datum #'bool-syn)])
       (and (boolean? bool)
            bool))]
    [_ #false]))

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

(define-syntax (grading-finished stx)
  (syntax-case stx ()
    [(_ bool)
     (and
      (check-synobj-satisfies boolean? #'bool 'grading-finished-entry "not a boolean")
      #'void)]
    [(_ arg arg-excess . args)
     (raise-syntax-error 'grading-finished-entry "too many arguments" stx #'arg-excess)]
    [(_) (raise-syntax-error 'grading-finished-entry "boolean missing in grading-finished entry" stx)]
    [_ (raise-syntax-error 'grading-finished-entry "incorrect grading-finished entry" stx)]))

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

          ; Don't throw out grading-finished entry, but check it.
          (tg-f)

          (define-syntax (gf-module-begin stx)
            (syntax-case stx ()
              [(_ (g-f exrcs (... ...)))
               (and
                (check-satisfies grading-finished-entry?
                                 #'g-f
                                 ; XXX We have a syntax error, but that's imprecise. Where's exactly the problem?
                                 'top-level
                                 "first item not a valid 'grading finished' entry")
                (if (= (length (syntax->datum #'(exrcs (... ...)))) #,(length maxp))
                    (when (andmap
                           (check-exercise (list #,@descr) (list #,@maxp) (grading-finished? #'g-f))
                           (syntax->list #'(exrcs (... ...)))
                           (range #,(length maxp)))
                      #'(#%module-begin (g-f #t)))
                    ; What should the source location be?
                    (raise-syntax-error 'top-level "wrong number of exercise entries")))]))))]))
