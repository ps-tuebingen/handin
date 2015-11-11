#lang racket/base

(require racket/list
         racket/function
         racket/bool
         racket/match)

(require unstable/list)
(require math/statistics)

(require "../handin-server/format-grade.rkt")
(define GRADE-FILENAME "grade.rktd")
(define DIRECTORY-SEARCH-DEPTH-LIMIT 3)

(define args (current-command-line-arguments))

; Point -> Bucket
(define (points->bucket points)
  (cond [(< points 10) 1]
        [(< points 20) 2]
        [(< points 30) 3]
        [(< points 40) 4]
        [(< points 50) 5]
        [(< points 60) 6]
        [(< points 70) 7]
        [(< points 80) 8]
        [(< points 90) 9]
        [else 10]))

; Bucket -> String
(define (bucket-name bucket)
  (cond [(= bucket 1) "<10"]
        [(= bucket 2) "10..19"]
        [(= bucket 3) "20..29"]
        [(= bucket 4) "30..39"]
        [(= bucket 5) "40..49"]
        [(= bucket 6) "50..59"]
        [(= bucket 7) "60..69"]
        [(= bucket 8) "70..79"]
        [(= bucket 9) "80..89"]
        [(= bucket 10) ">90"]))

; (List-of GradingScheme) -> (List-Of (Cons Bucket Number))
(define (grade-histogram gs)
  (let
      ((scores (map grading-scheme-total gs)))
    (map (lambda (xs) (cons (bucket-name (points->bucket (first xs))) (length xs)))
         (sort (group-by points->bucket scores) (lambda (x y) (< (first x) (first y)))))))



; Path -> (List-Of Path)
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

; Represents a GradingScheme and a StudentName
(define-struct grade (grading-scheme student-name))

; Path -> (List-of GradingScheme)
(define (all-grading-schemes wd)
  (map read-grading-scheme (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of Grade)
(define (all-grading-schemes* wd)
  (map (λ (p)
         (grade (read-grading-scheme p) (get-user-name-from-path p)))
       (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of GradingScheme)
(define (all-finished-grading-schemes wd)
  (filter finished-grading-scheme? (all-grading-schemes wd)))

; Path -> (List-of GradingRecord)
(define (all-finished-grading-schemes* wd)
  (filter (λ (gr)
            (finished-grading-scheme? (grade-grading-scheme gr)))
          (all-grading-schemes* wd)))

; Path -> (List-of Path)
(define (all-erroneous-grading-schemes wd)
  (filter (lambda (p) (erroneous-grading-scheme? (read-grading-scheme p)))
          (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of Path)
(define (all-unfinished-grading-schemes wd)
  (filter (lambda (p) (not (finished-grading-scheme? (read-grading-scheme p))))
          (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))


; Path -> StudentName
(define (get-user-name-from-path p)
  (let* ((path-components (explode-path p))
         (numOfPC (length path-components)))
    (if (> numOfPC 1)
        (list-ref path-components (- numOfPC 2))
        p)))

(define (list-unfinished wd)
  (let ((unfinished (all-unfinished-grading-schemes wd)))
    (begin
      (display (format "Total number of unfinished grading schemes: ~a\n" (length unfinished)))
      (for ([p unfinished])
        (display (format "~a " (get-user-name-from-path p))))
      (newline))))


(define (list-grades wd)
  (define finished (all-finished-grading-schemes* wd))
  (display (format "Total number of finished grading schemes: ~a\n" (length finished)))
  (for ([g finished])
    (display (format "~a: ~a\n"
                     (grade-student-name g)
                     (grading-scheme-total (grade-grading-scheme g))))))

(define (list-erroneous wd)
  (let ((erroneous (all-erroneous-grading-schemes wd))
        (morethan100points (filter (lambda (gs) (> (grading-scheme-total (grade-grading-scheme gs)) 100)) (all-finished-grading-schemes* wd) )))
    (begin
      (display (format "Total number of erroneous grading schemes: ~a\n" (length erroneous)))
      (for ([p erroneous])
        (display (format "~a " (get-user-name-from-path p))))
      (newline)
      (display (format "Number of grading schemes with more than 100 points: ~a\n" (length morethan100points)))
      (for ([p morethan100points])
        (display (format "~a " (grade-student-name p))))
      (newline))))

(define (stats wd)
  (let ((scores (map grading-scheme-total (all-finished-grading-schemes wd))))
    (begin
      (display (format "Number of finished grade files: ~a\n" (length scores)))
      (display (format "Mean score: ~a\n" (mean scores)))
      (display (format "Median score: ~a\n" (median < scores))))))

(define (histo wd)
  (for [( q (grade-histogram (all-finished-grading-schemes wd)))]
    (display (format "Point range ~a : ~a \n" (car q) (cdr q)))))

(define (usage)
  (display "usage: racket format-grade.rkt (stats|list|unfinished|erroneous|histogram) path\n"))

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
  [else (usage)])
