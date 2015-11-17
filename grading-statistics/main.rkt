#lang racket/base

(require racket/list
         racket/function
         racket/bool
         racket/match)

(require unstable/list)
(require math/statistics)

(require "../handin-server/format-grade.rkt")
(define GRADE-FILENAME "grade.rktd")
(define DIRECTORY-SEARCH-DEPTH-LIMIT 5)

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

; (List-of FinishedGradingTable) -> (List-Of (Cons Bucket Number))
(define (grade-histogram gs)
  (let
      ((scores (map grading-table-total gs)))
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

; A GradingRecord pairs a GradingTable and a StudentName
(define-struct grading-record (table name))

; A FinishedGradingRecord is a GradingRecord whose GradingTable is a FinishedGradingTable.

; Path -> (List-of GradingTable)
(define (all-grading-tables wd)
  (map read-grading-table (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of GradingRecord)
(define (all-grading-tables* wd)
  (map (λ (p)
         (grading-record (read-grading-table p) (get-user-name-from-path p)))
       (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of FinishedGradingTable)
(define (all-finished-grading-tables wd)
  (filter finished-grading-table? (all-grading-tables wd)))

; Path -> (List-of FinishedGradingRecord)
(define (all-finished-grading-tables* wd)
  (filter (λ (gr)
            (finished-grading-table? (grading-record-table gr)))
          (all-grading-tables* wd)))

; Path -> (List-of Path)
(define (all-erroneous-grading-tables wd)
  (filter (lambda (p) (erroneous-grading-table? (read-grading-table p)))
          (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of Path)
(define (all-unfinished-grading-tables wd)
  (filter (lambda (p) (not (finished-grading-table? (read-grading-table p))))
          (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))


; Path -> StudentName
(define (get-user-name-from-path p)
  (let* ((path-components (explode-path p))
         (numOfPC (length path-components)))
    (if (> numOfPC 1)
        (list-ref path-components (- numOfPC 2))
        p)))

(define (list-unfinished wd)
  (let ((unfinished (all-unfinished-grading-tables wd)))
    (begin
      (display (format "Total number of unfinished grading tables: ~a\n" (length unfinished)))
      (for ([p unfinished])
        (display (format "~a " (get-user-name-from-path p))))
      (newline))))


(define (list-grades wd)
  (define finished (all-finished-grading-tables* wd))
  (display (format "Total number of finished grading tables: ~a\n" (length finished)))
  (for ([g finished])
    (display (format "~a: ~a\n"
                     (grading-record-name g)
                     (grading-table-total (grading-record-table g))))))

(define (list-erroneous wd)
  (let ((erroneous (all-erroneous-grading-tables wd))
        (morethan100points (filter (lambda (gs) (> (grading-table-total (grading-record-table gs)) 100)) (all-finished-grading-tables* wd))))
    (begin
      (display (format "Total number of erroneous grading tables: ~a\n" (length erroneous)))
      (for ([p erroneous])
        (display (format "~a " (get-user-name-from-path p))))
      (newline)
      (display (format "Number of grading tables with more than 100 points: ~a\n" (length morethan100points)))
      (for ([p morethan100points])
        (display (format "~a " (grading-record-name p))))
      (newline))))

(define (verify wd schema)
  (let* ((gt (all-finished-grading-tables* wd))
         (noOfGrades (lambda (g) (- (length (grading-record-table g)) 1)))
         (wrongNoOfGrades (filter (lambda (g) (not (= (length schema) (noOfGrades g)))) gt))
         (rightNoOfGrades (filter (lambda (g) (= (length schema) (noOfGrades g))) gt))
         (tooManyPointsOnTask (filter (lambda (ge)
                                     (foldl (lambda (x y) (or x y))
                                             #f
                                             (map (lambda (gentry max) (> (second gentry) max)) (cdr (grading-record-table ge)) schema)))
                                   rightNoOfGrades)))
    (begin
      (display (format "Total number of grading tables with incorrect number of grades: ~a\n" (length wrongNoOfGrades)))
      (for ([p wrongNoOfGrades])
        (display (format "~a " (grading-record-name p))))
      (newline)
      (display (format "Total number of grading tables with too many points on some tasks: ~a\n" (length tooManyPointsOnTask)))
      (for ([p tooManyPointsOnTask])
        (display (format "~a " (grading-record-name p))))
      (newline))))




(define (stats wd)
  (let ((scores (map grading-table-total (all-finished-grading-tables wd))))
    (begin
      (display (format "Number of finished grade files: ~a\n" (length scores)))
      (display (format "Mean score: ~a\n" (mean scores)))
      (display (format "Median score: ~a\n" (median < scores))))))

(define (histo wd)
  (for [( q (grade-histogram (all-finished-grading-tables wd)))]
    (display (format "Point range ~a : ~a \n" (car q) (cdr q)))))

(define (parse-schema s)
  (let ((schema (read (open-input-string s))))
    (if (and
         (list? schema)
         (for/and [(m schema)] (number? m)))
        schema
        (error (format "Cannot parse grade schema, must be list of numbers: ~a" schema)))))


(define (usage)
  (display "usage: racket -l grading-statistics (stats|list|unfinished|erroneous|verify|histogram) <path> <schema>\n"))

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
  ["verify" (verify working-directory (parse-schema (vector-ref args 2)))]
  [else (usage)])
