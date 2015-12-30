#lang racket/base

(require racket/match
         "../handin-server/format-grade.rkt"
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
  (let ([schema-file (build-path working-directory SCHEMA-FILENAME)])
    (and (file-exists? schema-file)
         (or (read-grading-table schema-file)
             (display-error (format "Problem loading schema file: ~a" schema-file))))))

(define student
  (if (< (vector-length args) 3)
      #f
      (vector-ref args 2)))

(define exercise-no
  (if (< (vector-length args) 3)
      #f
      (string->number (vector-ref args 2))))

(match (vector-ref args 0)
  ["stats" (stats working-directory)]
  ["list" (list-grades working-directory)]
  ["unfinished" (list-unfinished working-directory)]
  ["erroneous" (list-erroneous working-directory)]
  ["histogram" (histo working-directory)]
  ["histo-by-studiengang" (histo-by-studiengang working-directory)]
  ["stats-by-studiengang" (stats-by-studiengang working-directory)]
  ["means-list" (means-list working-directory)]
  ["student-scores" (if student
                        (display-student-scores student working-directory)
                        (display-error "Please specify which student"))]
  ["performance-drops" (if student
                           (display-performance-drops student working-directory)
                           (display-error "Please specify which student"))]
  ["pdrop-students" (display-pdrop-students working-directory)]
  ["means-per-exercise" (means-per-exercise working-directory)]
  ["histo-for-exercise" (if exercise-no
                            (histo-for-exercise exercise-no working-directory)
                            (display-error "Please specify which exercise"))]
  
  
  ["verify" (if schema
                (verify working-directory (parse-schema schema))
                (display-error
                 (string-append "Schema file not found, searched for " SCHEMA-FILENAME " in " (path->string working-directory))))]
  [else (usage)])
