#lang racket/base

(require racket/match
         "grade-eval-utils.rkt")

; this file simply contains the max. points schema
(define SCHEMA-FILENAME "schema.rktd")

(define args (current-command-line-arguments))

(define (usage)
  (display "usage: racket -l grading-statistics (stats|list|unfinished|erroneous|verify|histogram|histo-by-studiengang|stats-by-studiengang) <path>\n"))

(define (display-error e)
  (begin
    (display (string-append e "\n"))
    (usage)
    (exit 1)))

(if (< (vector-length args) 2)
    (display-error "Wrong number of arguments")
    (void))

(define working-directory
  (if (directory-exists? (string->path (vector-ref args 1)))
      (string->path (vector-ref args 1))
      (display-error (format "Path not found: ~a" (vector-ref args 1)))))

(define schema
  (let ((schema-file (build-path working-directory SCHEMA-FILENAME)))
    (and (file-exists? schema-file)
         (with-handlers ([exn:fail?
                          (lambda (exn) (display-error (format "Problem loading schema file: ~a" schema-file)))])
           (call-with-input-file* schema-file
             (λ (input-port)
               ; Workaround Racket bug https://github.com/racket/racket/issues/1114
               (when (equal? (peek-char input-port) #\uFEFF)
                 (read-char input-port))
               ; Skip Racket header (lines that start with ; or #), if present
               (letrec ((skipheader (lambda ()
                                      (when (or (equal? (peek-char input-port) #\;)
                                                (equal? (peek-char input-port) #\#))
                                        (begin
                                          (read-line input-port)
                                          (skipheader))))))
                 (skipheader))
               
               (read input-port)))))))

(match (vector-ref args 0)
  ["stats" (stats working-directory)]
  ["list" (list-grades working-directory)]
  ["unfinished" (list-unfinished working-directory)]
  ["erroneous" (list-erroneous working-directory)]
  ["histogram" (histo working-directory)]
  ["histo-by-studiengang" (histo-by-studiengang working-directory)]
  ["stats-by-studiengang" (stats-by-studiengang working-directory)]
  
  
  
  ["verify" (if schema
                (verify working-directory (parse-schema schema))
                (display-error
                 (string-append "Schema file not found, searched for " SCHEMA-FILENAME " in " (path->string working-directory))))]
  [else (usage)])
