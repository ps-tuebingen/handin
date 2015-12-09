#lang racket/base

(require racket/match
         "grade-eval-utils.rkt")

(define args (current-command-line-arguments))

(define (usage)
  (display "usage: racket -l grading-statistics (stats|list|unfinished|erroneous|verify|histogram|histo-by-studiengang|stats-by-studiengang) <path> <schema>\n"))

(if (< (vector-length args) 2)
    (begin
      (display "Wrong number of arguments\n")
      (usage)
      (exit 1))
    (void))

(define working-directory
  (if (directory-exists? (string->path (vector-ref args 1)))
      (string->path (vector-ref args 1))
      (begin
        (display (format "Path not found: ~a\n" (vector-ref args 1)))
        (usage)
        (exit 1))))

(match (vector-ref args 0)
  ["stats" (stats working-directory)]
  ["list" (list-grades working-directory)]
  ["unfinished" (list-unfinished working-directory)]
  ["erroneous" (list-erroneous working-directory)]
  ["histogram" (histo working-directory)]
  ["histo-by-studiengang" (histo-by-studiengang working-directory)]
  ["stats-by-studiengang" (stats-by-studiengang working-directory)]

  

  ["verify" (verify working-directory (parse-schema (vector-ref args 2)))]
  [else (usage)])
