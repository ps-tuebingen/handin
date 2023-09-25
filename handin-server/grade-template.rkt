#lang racket/base

(require (for-syntax racket))
(require (for-syntax racket/base))

; Define language for grade-template.rktd files as a variant of racket/base
; with a special #%module-begin, so that running correct files will not
; produce strange warnings or behaviors.
(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [gf-def-module-begin #%module-begin]))

; (SyntaxObject -> Boolean) SyntaxObject (U Symbol #f) String -> Boolean
;
; Check that syntax object synob satisfies predicate pred, otherwise raise an
; error.
(define-for-syntax (check-satisfies pred synob source errstr)
  (if (pred synob)
      #true
      (raise-syntax-error source errstr synob)))


; (Datum -> Boolean) SyntaxObject (U Symbol #f) String -> Boolean
;
; Check that syntax object synob, after being converted through `syntax->datum`,
; satisfies predicate pred, otherwise raise an error.
(define-for-syntax (check-synobj-satisfies pred synob source errstr)
  (check-satisfies (λ (synob) (pred (syntax->datum synob))) synob source errstr))


; SyntaxObject (U Symbol #f) String -> Boolean
; Test whether given description is valid.
(define-for-syntax (valid-description? synd current-description)
  (and
    (check-synobj-satisfies string? synd 'exercise-entry "description not string")
    (check-synobj-satisfies (λ (descr-tested) (string=? descr-tested current-description))
                            synd
                            'exercise-entry
                            "description doesn't match template")))

; SyntaxObject SyntaxObject (U Symbol #f) -> Boolean
; Check whether syntax objects constitute valid feedback entry.
(define-for-syntax (valid-feedback? synd synp)
  (and
    (string-prefix? (syntax->datum synd) "Feedback")
    (check-synobj-satisfies string?
                            synp
                            'exercise-entry
                            "feedback must be a string")))

; SyntaxObject SyntaxObject (U Symbol #f) Number -> Boolean
; Check whether syntax objects constitute valid score entry.
(define-for-syntax (valid-score? synd synp current-max-score)
  (and
    (string-prefix? (syntax->datum synd) "Bewertung")
    (check-synobj-satisfies exact-integer? synp 'exercise-entry "score must be an integer")
    (check-synobj-satisfies exact-nonnegative-integer? synp 'exercise-entry "score not >= 0")
    (check-synobj-satisfies (λ (score) (<= score current-max-score))
                            synp
                            'exercise-entry
                            "overly high score on exercise")))

; Datum -> Boolean
; Check whether Datum is valid bullet symbol.
(define-for-syntax (bullet-symbol? bullet)
  (and
    (symbol? bullet)
    (or (symbol=? bullet '-)
        (symbol=? bullet 'o)
        (symbol=? bullet '+))))

; SyntaxObject (U Symbol #f) -> Boolean
; Check whether syntax object constitutes valid bullet entry.
(define-for-syntax (valid-bullet? synp)
  (check-synobj-satisfies bullet-symbol?
                          synp
                          'exercise-entry
                          "bullet must be -, o or +"))

; (List-Of String) (List-Of Number) Boolean -> (SyntaxObject Number -> Boolean)
; Validate a grading entry in a student file (described by stx) according to the
; i-th entry of the grading template (as described in lists descriptions and max-scores).
(define-for-syntax (check-exercise descriptions max-scores finished-grading)
  (lambda (stx i)
    (let ([current-description (list-ref descriptions i)]
          [current-max-score (list-ref max-scores i)])
      (syntax-case stx ()
        [(d p)
         (and
          (valid-description? #'d current-description)
          (or
            ; Allow symbols in unfinished grade files
            ; (see https://github.com/ps-tuebingen/info1-teaching-material/issues/92).
            (and (not finished-grading) (symbol? (syntax->datum #'p)))
            (valid-feedback? #'d #'p)
            (valid-score? #'d #'p current-max-score)
            (valid-bullet? #'p)))]))))

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
(define-for-syntax (get-description stx)
  (syntax-case stx ()
    [(d a p) (syntax->datum #'d)]))

; List-of Syntax -> List-of Integer
; (syntax of the form (string-literal symbol integer-literal))
(define-for-syntax (get-max-score stx)
  (syntax-case stx ()
    [(d a p) (syntax->datum #'p)]))

(define-for-syntax (check-grading-finished-key key)
  (check-synobj-satisfies (λ (key)
                            (symbol=? key 'grading-finished))
                          key
                          'grading-finished-entry
                          "key of the first entry should be 'grading-finished"))

; every exercise is now 2 points max and there may be 3 or 4 on a homework
(define-for-syntax EXPECTED-TOTAL-SCORE1 6)
(define-for-syntax EXPECTED-TOTAL-SCORE2 8)
(define-for-syntax (validate-total-scores max-scores)
  (when (not (or (= (apply + max-scores) EXPECTED-TOTAL-SCORE1) (= (apply + max-scores) EXPECTED-TOTAL-SCORE2)))
    ; What should the source location be?
    (raise-syntax-error 'top-level (~a "total score is not " EXPECTED-TOTAL-SCORE1 " or " EXPECTED-TOTAL-SCORE2))))

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

(define-for-syntax ((module-checker descriptions max-scores) stx)
  (syntax-case stx ()
    [(_ (g-f exrcs ...))
     (and
      (grading-finished-checker #'g-f #:is-template #false)
      (if (= (length (syntax->datum #'(exrcs ...))) (length max-scores))
          (when (andmap
                 (check-exercise descriptions max-scores (grading-finished? #'g-f))
                 (syntax->list #'(exrcs ...))
                 (range (length max-scores)))
            #'(#%module-begin))
          ; What should the source location be?
          (raise-syntax-error 'top-level "wrong number of exercise entries")))]))

; Validate the grade-template, and macro-expand it into the appropriate language
; definition.
(define-syntax (gf-def-module-begin stx)
  (syntax-case stx ()
    [(_ (tg-f texrcs ...))
     (let* ([stxlist (syntax->list #'(texrcs ...))]
            [descriptions (map get-description stxlist)]
            [max-scores (map get-max-score stxlist)])
       (begin
         (grading-finished-checker #'tg-f #:is-template #true)
         (validate-total-scores max-scores)

         ; Define language for grade.rktd files as a variant of racket/base
         ; with a special #%module-begin, so that running correct files will not
         ; produce strange warnings or behaviors.
         #`(#%module-begin

            (require racket/base) ; Needed for the reexport.
            (provide
             (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [gf-module-begin #%module-begin]))

            (define-syntax gf-module-begin
              (module-checker (list #,@descriptions) (list #,@max-scores))))))]))
