#lang racket
(require handin-server/checker)
(provide (all-defined-out))

(define user-error-message
  (string-append "We cannot run your code.\n\n"
                  "Please fix the error before submitting.\n"
                  "Hit \"Run\" and debug your code.\n\n"
                  "This is the error we got:\n\n"
                  "~a\n"))

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
    (error* (string-append "We cannot find the constant '~a'.\n\n"
                           "Remember to program your expression like this:\n\n"
                           "  (define ~a\n"
                           "    (... your expression here ...))\n\n"
                           "This is the error we got:\n\n"
                           "~a\n")
            symbol symbol
            (exn-message e))))

(define-syntax-rule (extract-from-handin name body ...)
  (let ([name
         (with-handlers
             ([exn:fail? (handle-not-found 'name)])
           (!eval name))])
    body ...))
