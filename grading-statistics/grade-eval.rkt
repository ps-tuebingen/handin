#lang racket/base

(require racket/list
         racket/function
         racket/bool
         racket/match)

(require math/statistics)

(require "../handin-server/format-grade.rkt")
(define GRADE-FILENAME "grade.rktd")
(define DIRECTORY-SEARCH-DEPTH-LIMIT 3)

(define args (current-command-line-arguments))

(define (find-all-grade-files dir-or-file max-depth)
 (if (<= max-depth 0)
  (list)
  (if (equal?
       (string->path GRADE-FILENAME)
       (let-values
          ([(_1 filename _2) (split-path dir-or-file)]) filename))
      (list dir-or-file)
      (if (directory-exists? dir-or-file)
          (apply append (map (lambda (p) (find-all-grade-files p (- max-depth 1))) (directory-list dir-or-file #:build? #t)))
          (list)))))


(define (all-finished-grading-schemes wd)
  (filter finished-grading-scheme? (map read-grading-scheme (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT))))

(define (list-grades wd)
    (for ([ g (all-finished-grading-schemes wd)])
      (display (format "~a\n" (grading-scheme-total g)))))


(define (stats wd)
  (let ((scores (map grading-scheme-total (all-finished-grading-schemes wd))))
    (begin
      (display (format "Number of finished grade files: ~a\n" (length scores)))
      (display (format "Mean score: ~a\n" (mean scores)))
      (display (format "Median score: ~a\n" (median < scores)))
      )))

(define (usage)
  (display "usage: racket format-grade.rkt (average|list) path\n"))

(if (< (vector-length args) 2)
    (begin
      (display "Wrong number of arguments\n")
      (usage)
      (exit 1)
      )
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
  [else (usage)])

