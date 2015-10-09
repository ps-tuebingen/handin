#lang racket
(require handin-server/checker)
(provide (all-defined-out))

; Translate a language name to something we could show to the user.
; Currently stubbed and not really used.
(define (lang-display-name string)
  string)

(define (user-error-message underlying-message)
  (define match-result
    (regexp-match
     #rx"^make-module-evaluator: module code used `([^']*)' for a language, expecting `([^']*)'"
     underlying-message))
  ; Fail the submission in this case.
  (raise-user-error
   (if
    match-result
    (local
      ; These definitions are available to the actual message.
      [(define used-lang (lang-display-name (second match-result)))
       (define expected-lang (lang-display-name (third match-result)))]
      (format "You have selected the wrong language. Please fix according to instructions and resubmit."))
    (format
     (string-append "We cannot run your code.\n\n"
                    "Please fix the error before submitting.\n"
                    "Hit \"Run\" and debug your code.\n\n"
                    "This is the error we got:\n\n"
                    "~a\n") underlying-message))))

(define saved-grades (make-hash))
(define (start-grading)
  ; Copy handin.rkt to Korrektur.rkt
  (void))

(define GRADE-TEMPLATE-PATH "../../grade-template.rktd")
; Save grades
(define (end-grading)
  (with-output-to-file
      "grade.rktd"
    (λ () (display (for/fold ([grade (port->string (open-input-file GRADE-TEMPLATE-PATH))])
                             ([(item-name item-grade) saved-grades])
                     (string-replace grade item-name (number->string item-grade)))))))

; Save item-grade as grade for item-name
(define (set-grade item-name item-grade)
  (hash-set! saved-grades item-name item-grade))

(define (error* fmt . args)
  (message (apply format fmt args) '(ok caution)))

(define (handle-not-found symbol)
  (lambda (e)
    (error* (string-append "Warning: We cannot find the constant '~a'.\n\n"
                           "Remember to program your expression like this:\n\n"
                           "  (define ~a\n"
                           "    (... your expression here ...))\n\n"
                           "This is the error we got:\n\n"
                           "~a\n")
            symbol symbol
            (exn-message e))
    (cons #f '())))

(define-syntax-rule (extract-from-handin name body ...)
  (let* ([result (with-handlers
                     ([exn:fail? (handle-not-found 'name)])
                   (cons #t (!eval name)))]
         [successful-eval (car result)]
         [name (cdr result)]) ; Name is only valid if successful-eval.
    (when successful-eval
      body ...)))
