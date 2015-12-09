#lang racket/base

(provide editors->string
         first-line-rest)

(require racket/class
         racket/string
         racket/gui/base)

(module+ test
  (require rackunit))

; Separated for reusability. Should still stay on handin-client to be included in deploys of the plugin.
(define (editors->string definitions interactions)
  (let* ([base (make-object editor-stream-out-bytes-base%)]
         [stream (make-object editor-stream-out% base)])
    (write-editor-version stream base)
    (write-editor-global-header stream)
    (for ([ed (in-list (list definitions interactions))]) (send ed write-to-file stream))
    (write-editor-global-footer stream)
    (send base get-bytes)))

; first-line-rest : String -> String (+ String #f)
; returns its argument split into the first line and the rest. If there's only one line, the second component is #f
(module+ test
  (define (first-line-rest-law test-string)
    (define-values (first-line rest-lines) (first-line-rest test-string))
    (define joined-back (string-join (if rest-lines
                                         (list first-line rest-lines)
                                         (list first-line))
                                     "\n"))
    (check-equal? joined-back test-string))

  (first-line-rest-law "")
  (first-line-rest-law "\n")
  (first-line-rest-law "\n\n")
  (first-line-rest-law "\n\n\n")
  (first-line-rest-law "hi!")
  (first-line-rest-law "hi!\n")
  (first-line-rest-law "hi!\n\n")
  (first-line-rest-law "hi!\n\n\n")
  (first-line-rest-law " \na\nb")
  (first-line-rest-law " \na\nb\n\n\n")

  (define (test-first-line-is test-string expected-first-line)
    (define-values (first-line rest-lines) (first-line-rest test-string))
    (check-equal? first-line expected-first-line))
  (test-first-line-is "a..sadsad\na\nb" "a..sadsad")
  (test-first-line-is " \na\nb" " ")
  (test-first-line-is " foo " " foo "))

(define (first-line-rest string)
  (define components (string-split string "\n" #:trim? #f #:repeat? #f))
  (if (null? components)
      (values "" #f)
      (values
       (car components)
       (let ([components-rest (cdr components)])
         (if (null? components-rest)
             #f
             (string-join components-rest "\n"))))))
