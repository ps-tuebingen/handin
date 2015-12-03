#lang racket/base

; Provide utilities for main.rkt. They're not in main.rkt here so they can be
; tested, via Rackunit or the REPL.

(provide save-submission)
(require racket/file
         racket/path)

(module+ test
  (require rackunit))

(module+ test
  ; Tests for tmp-filename-for
  ; Relative paths
  (check-equal? (tmp-filename-for "a") (build-path (current-directory) ".tmp-a"))
  (check-equal? (tmp-filename-for "./a") (build-path (current-directory) ".tmp-a"))
  (check-equal? (tmp-filename-for (build-path (current-directory) "a")) (build-path (current-directory) ".tmp-a"))

  ; Absolute paths
  (check-equal? (tmp-filename-for "/a") (string->path "/.tmp-a"))
  (check-equal? (tmp-filename-for "/a/b") (string->path "/a/.tmp-b"))
  (check-equal? (tmp-filename-for "/path/a") (string->path "/path/.tmp-a"))

  ; Paths with ..
  (check-equal?
   (tmp-filename-for "../path/a")
   (simple-form-path (build-path (current-directory) "../path/.tmp-a"))))

; path-string? -> path?
(define (tmp-filename-for path)
  (define-values (base-path filename _) (split-path (simple-form-path path)))
  (build-path base-path (format ".tmp-~a" (path->string filename))))

; bytes? path-string? -> void?
; Atomically save s to a file named part
;
; Implementation notes: for atomicity, this avoids call-with-atomic-output-file,
; since I don't trust it because of its bugs:
; https://github.com/racket/racket/issues/1156.
;
; Here I use a huge simplification of `call-with-atomic-output-file`, which is
; unsafe in the general case:
;
; - I reimplement temporary filename creation. That's fine in our controlled
;   environment.
;
; - I don't delete temporary files on exceptions -- I don't expect exceptions
;   here (as long as satisfies its bytes? contract).
(define (save-submission s part)
  (define tmp-filename (tmp-filename-for part))
  (with-output-to-file
   tmp-filename
   (lambda () (display s)))
  (rename-file-or-directory tmp-filename part))
