#lang racket/base

(require (for-syntax racket))
(require (for-syntax racket/base))

; Define language for grade-template.rktd files as a variant of racket/base
; with a special #%module-begin, so that running correct files will not
; produce strange warnings or behaviors.
(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [gf-def-module-begin #%module-begin]))

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

; Check if the passed grading-finished entry marks the file as complete.
(define-for-syntax (grading-finished? stx)
  (syntax-case stx ()
    [(id bool-syn)
     (let ([bool (syntax->datum #'bool-syn)])
       (and
        (symbol=? (syntax->datum #'id) 'grading-finished)
        (boolean? bool)
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

(define-for-syntax (check-grading-finished-key key)
  (check-synobj-satisfies (λ (key)
                            (symbol=? key 'grading-finished))
                          key
                          'grading-finished-entry
                          "key of the first entry should be 'grading-finished"))

(define-for-syntax EXPECTED-TOTAL-SCORE 100)
(define-for-syntax (validate-total-points maxp)
  (when (not (= (apply + maxp) 100))
    ; What should the source location be?
    (raise-syntax-error 'top-level (~a "total score is not " EXPECTED-TOTAL-SCORE))))

(define-for-syntax (grading-finished-checker stx #:is-template is-template)
  (syntax-case stx ()
    [(key bool)
     (and
      (check-grading-finished-key #'key)
      (check-synobj-satisfies boolean? #'bool 'grading-finished-entry "not a boolean")
      (or (not is-template)
          (check-synobj-satisfies false? #'bool 'grading-finished-entry
                                  "grading-finished value must be #false in template"))
      #'(void))]
    [(key arg arg-excess . args)
     (and
      (check-grading-finished-key #'key)
      (raise-syntax-error 'grading-finished-entry "too many arguments in grading-finished entry" stx #'arg-excess))]
    [(key)
     (and
      (check-grading-finished-key #'key)
      (raise-syntax-error 'grading-finished-entry "boolean missing in grading-finished entry" stx))]
    [_
     (raise-syntax-error 'grading-finished-entry "incorrect grading-finished entry" stx)]))

; Macro generating macro checking language
; ----------------------------------------
(define-syntax (gf-def-module-begin stx)
  (syntax-case stx ()
    [(_ (tg-f texrcs ...))
     (let* ([stxlist (syntax->list #'(texrcs ...))]
            [descr (map description stxlist)]
            [maxp (map maxpoints stxlist)])
       (begin
         (grading-finished-checker #'tg-f #:is-template #true)
         (validate-total-points maxp)

         ; Define language for grade.rktd files as a variant of racket/base
         ; with a special #%module-begin, so that running correct files will not
         ; produce strange warnings or behaviors.
         #`(#%module-begin

            (require racket/base) ; Needed for the reexport.
            (provide
             (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [gf-module-begin #%module-begin]))

            (define-syntax (gf-module-begin stx)
              (syntax-case stx ()
                [(_ (g-f exrcs (... ...)))
                 (and
                  (grading-finished-checker #'g-f #:is-template #false)
                  (if (= (length (syntax->datum #'(exrcs (... ...)))) #,(length maxp))
                      (when (andmap
                             (check-exercise (list #,@descr) (list #,@maxp) (grading-finished? #'g-f))
                             (syntax->list #'(exrcs (... ...)))
                             (range #,(length maxp)))
                        #'(#%module-begin))
                      ; What should the source location be?
                      (raise-syntax-error 'top-level "wrong number of exercise entries")))])))))]))
