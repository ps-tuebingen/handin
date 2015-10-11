#lang racket/base
(require racket/local
         racket/list
         racket/string
         racket/port
         handin-server/checker 2htdp/image)
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
                     (string-replace grade item-name (number->string item-grade))))))
  (define warning-buffer (thread-cell-ref warning-buffer-cell))
  (when (not (string=? warning-buffer ""))
    (message warning-buffer '(ok caution))))

; Save item-grade as grade for item-name
(define (set-grade item-name item-grade)
  (hash-set! saved-grades item-name item-grade))

; Saves the warnings for a submission.
; This variable is created per module, that is, per handin-server.
; To ensure the buffer is not shared across submissions, we use a per-thread
; variable, since checkers for different submissions run in different threads.
; I took the idea from add-report-line!.
(define warning-buffer-cell (make-thread-cell ""))

(define (error* fmt . args)
  (define fmt-msg (apply format fmt args))
  (thread-cell-set! warning-buffer-cell (string-append (thread-cell-ref warning-buffer-cell) fmt-msg "\n")))

(define (handle-not-found symbol)
  (lambda (e)
    (error* (string-append "Warning: We cannot find the constant '~a'.\n\n"
                           "Remember to program your expression like this:\n\n"
                           "  (define ~a\n"
                           "    (... your expression here ...))\n")
            symbol symbol)
    (cons #f '())))

(define-syntax-rule (extract-from-handin name body ...)
  (let* ([result (with-handlers
                     ([exn:fail? (handle-not-found 'name)])
                   (cons #t (!eval name)))]
         [successful-eval (car result)]
         [name (cdr result)]) ; Name is only valid if successful-eval.
    (when successful-eval
      body ...)))

(define-syntax-rule (extract-from-handin/typed type-pred? typename name body ...)
  (extract-from-handin
   name
   (if (not (type-pred? name))
       (error* (string-append "We cannot understand the value of '~a'.\n"
                              "  We expect: ~a.\n"
                              "  We found: ~.v\n\n"
                              "Remember to program your picture like this:\n\n"
                              "  (define ~a\n"
                              "    (... your expression here ...))\n\n")
               'name
               typename
               'name
               name)
       (begin
         (void)
         body ...))))

(define-syntax-rule (extract-from-handin/image name body ...)
  (extract-from-handin/typed image? "an image" name body ...))
(define-syntax-rule (extract-from-handin/number name body ...)
  (extract-from-handin/typed number? "a number" name body ...))
(define-syntax-rule (extract-from-handin/string name body ...)
  (extract-from-handin/typed string? "a string" name body ...))

(require (only-in racket/gui/base open-input-text-editor))
; Not used here, but potentially useful elsewhere.
(define (get-definitions-port submission)
  (let-values ([(definitions interactions) (unpack-submission submission)])
    (open-input-text-editor definitions)))
