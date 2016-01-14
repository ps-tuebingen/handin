#lang racket/base

(provide (all-defined-out))
(require racket/list)
(require racket/math)
(require racket/function)
(require unstable/list)
(require racket/bool)
(require math/statistics)
(require plot)

(require "../handin-server/format-grade.rkt")
(require "utils.rkt")
(require "grade-eval-utils.rkt")

; Individual student-based grade evaluation
; =========================================
; =========================================

; A StudentScore consists of
; - a Path: the exercise folder.
; - a Bool: When the student handed something in it is #t, otherwise #f.
; - a U Points Bool: Only relevant when the second part is #t. In this case it is the points
; the student achieved - unless grading isn't finished/erroneous, then it is #f.
(define-struct student-score (path handin? points))

; String Path -> StudentScore
; The student's score according to the student's directory sd (directly contains grade file)
; Optional argument: the name of the homework
(define (retrieve-student-score sd)
  (let ([f (list-ref (reverse (explode-path sd)) 1)])
    (if (directory-exists? sd)
        (let ([grade-files (find-all-grade-files sd 0)])
          (if (> (length grade-files) 0)
              (let ([grading-table (read-grading-table (car grade-files))])
                (if (and (valid-grading-table? grading-table)
                         (finished-grading-table? grading-table))
                    (make-student-score f #t (grading-table-total (read-grading-table (car grade-files))))
                    (make-student-score f #t #f)))
              (make-student-score f #t #f)))
        (make-student-score f #f #f))))

; Path -> List-of Path
; The list of all homework folders in the wd
(define (homework-folders wd)
  (define (filename p) (call-with-values
                        (lambda () (split-path p))
                        (lambda (b n d) (path->string n))))
  (define (is-homework-folder? p)
    (and (directory-exists? p)
         (char-numeric? (first (string->list (filename p))))))
  (map filename (filter is-homework-folder? (directory-list wd #:build? #t))))

; String Path -> List-of StudentScore
; The list of all scores for the given student (over all homework subdirectories in the wd)
(define (student-scores s wd)
  (map (lambda (d) (retrieve-student-score (build-path wd d s))) (homework-folders wd)))

(define (display-student-scores s wd)
  (for ([scr (student-scores s wd)])
    (let ([exercise-name (student-score-path scr)])
    (if (student-score-handin? scr)
        (if (student-score-points scr)
            (display (format "~a : score : ~a %\n" exercise-name (student-score-points scr)))
            (display (format "~a : unfinished or invalid grading table\n" exercise-name)))
        (display (format "~a : no homework handed in\n" exercise-name))))))

; Path (U #f Points) (U #f Points) -> List-of String
; List all students that have a graded handin, respective to the given homework hw
; With the optional arguments min and max, one can exclude students which have average (over all homeworks)
; grades below or above a certain number of points
(define (students-with-graded-handins wd hw [min 0] [max +inf.0])
  (filter (lambda (s) (and
                       (student-score-points (retrieve-student-score (build-path wd hw s)))
                       (let ([m (mean (filter (negate false?) (map student-score-points (student-scores s wd))))])
                         (and (> m min) (< m max)))))
          (remove-duplicates (map get-user-name-from-path (find-all-grade-files (build-path wd hw) 1)))))

; Path -> List-of String
; List all students that have a graded handin for any of the homeworks
(define (students-with-any-graded-handin wd)
  (remove-duplicates (append-map (lambda (hw) (students-with-graded-handins wd hw))
                                 (homework-folders wd))))

; Performance drops
; =================

; A performance drop consists of
; - a Path: the exercise folder for the exercise after which the drop occurs (ex. a)
; - a Path: the exercise folder for the exercise towards which the drop occurs (ex. b)
; - a Number: (points for ex. b) / (points for ex. a)
(define-struct performance-drop (patha pathb ratio))

; String Rational Path -> List-of PerformanceDrop
; Returns all performance drops for the given student where the ratio is below threshold t.
(define (performance-drops s t wd)
  (let ([scores (student-scores s wd)])
    (filter (negate void?)
            (for/list ([i (range (- (length scores) 1))])
              (let* ([scr (list-ref scores i)]
                     [nextscr (list-ref scores (+ i 1))]
                     [scr-points (student-score-points scr)]
                     [nextscr-points (student-score-points nextscr)])
                (when (and (student-score-handin? scr) scr-points
                           (student-score-handin? nextscr) nextscr-points
                           (> scr-points 0))
                  (let ([perf-ratio (/ nextscr-points scr-points)])
                    (when (< perf-ratio t)
                      (performance-drop
                       (student-score-path nextscr)
                       (student-score-path scr)
                       perf-ratio)))))))))

(define (display-performance-drops s t wd)
  (begin (display (format "(Threshold: ~a)\n" t))
         (let ([pdrops (performance-drops s t wd)])
           (if (empty? pdrops)
               (display "No performance drops.\n")
               (for ([p (performance-drops s t wd)])
                 (display (format "~a : performance drop to ~a % of previous (~a)\n"
                                  (performance-drop-patha p)
                                  (real->decimal-string (percentify (performance-drop-ratio p)))
                                  (performance-drop-pathb p))))))))

; Path Rational -> List-of String
; Returns all students with at least one performance drop where the ratio is below threshold t.
(define (pdrop-students t wd)
  (remove-duplicates
   (filter (negate void?)
           (for/list ([student (remove-duplicates (map grading-record-name (all-finished-grading-tables* wd)))])
             (when (not (empty? (performance-drops student t wd)))
               student)))))

(define (display-pdrop-students t wd)
  (begin
    (display (format "(Threshold: ~a)\n" t))
    (for ([s (pdrop-students t wd)])
      (display (format "~a\n" s)))))

; # of handins effects
; ====================
;; UNDER CONSTRUCTION 

; Path -> Bool
; Whether the given file is a handin directory in the wd, i.e. a directory named SUCCESS-x, with x one of 0,...,9
(define (handin-dir? wd)
  (lambda (p)
    (and (directory-exists? (build-path wd p))
         (regexp-match #rx"^SUCCESS-[0-9]$" (path->string p)))))

; String Path -> Natural
; How often the student handed in for the respective homework hw, false when there is no directory for this student
(define (number-of-handins s wd hw)
  (let ([student-directory (build-path (build-path wd hw) s)])
    (if (directory-exists? student-directory)
        (length (filter (handin-dir? student-directory) (directory-list student-directory)))
        #f)))

; Approach A:
; -----------
; fix the homework, then consider how scores and #handins correlate (calculated over all students)

; Path Path -> List-of Natural
; Lists the numbers of handins for the respective homework hw for all students which have handed in this hw
; The optional arguments min and max are the same as for student-with-graded-handins
; and allow to exclude certain students.
(define (list-numbers-of-handins wd hw [min 0] [max +inf.0])
  (map (lambda (s) (number-of-handins s wd hw))
       (students-with-graded-handins wd hw min max)))

; Path -> List-of Points
; Lists the scores for the respective homework hw for all students
; The students' order is identical to that returned by list-numbers-of-handins.
; The optional arguments min and max are the same as for student-with-graded-handins
; and allow to exclude certain students.
(define (list-points wd hw [min 0] [max +inf.0])
  (map (lambda (s) (student-score-points (retrieve-student-score (build-path (build-path wd hw) s))))
       (students-with-graded-handins wd hw min max)))

; Path -> Real
; Correlation between number of handins and grades for the homework hw
(define (handin-count-grade-correlation wd hw [min 0] [max +inf.0])
  (correlation (list-points wd hw min max) (list-numbers-of-handins wd hw min max)))

; Plot points (y-axis) vs. handin counts (x-axis)
(define (plot-points-vs-handin-counts wd hw)
  (plot (points (map vector
                     (list-numbers-of-handins wd hw)
                     (list-points wd hw)))))

; Approach A.1:
; Plots for median grades for each handin count

; Natural Path Path -> Real
; Median points for students with handin count c
(define (median-points-for-handin-count c wd hw)
  (let* ([handin-numbers (list-numbers-of-handins wd hw)]
         [indices (filter (lambda (i) (= (list-ref handin-numbers i) c)) (range (length handin-numbers)))]
         [points (list-points wd hw)])
    (median < (map (lambda (i) (list-ref points i)) indices))))

; The maximum possible number of handins per homework for a student
(define MAX-HANDINS 10)

; Plot median grades per handin count, and additionally a polynomial regression curve of grade n
; Optional arguments: the minimum and maximum handin numbers to be considered
(define (plot-polyreg-median-points-per-handin-count n wd hw [min 1] [max MAX-HANDINS])
  (let ([xs (range min (+ max 1))]
        [ys (map (lambda (c) (median-points-for-handin-count c wd hw)) (range min (+ max 1)))])
    (plot (list (points   (map vector xs ys))
                (function (regression-polynomial xs ys n)))
          #:x-label "# of handins"
          #:y-label "median grade")))

; Approach B:
; -----------
; consider the mean # of handins and mean score of each student and how these means correlate

; String Path -> List-of Natural
; Lists all handin counts for the given student
(define (all-handin-counts-for-student s wd)
  (map (lambda (d) (number-of-handins s d)) homework-folders))

; Path -> List-of Real
; List the mean handin counts for all students
(define (mean-handin-counts wd)
  (map (lambda (s) (mean (filter (negate false?)
                                 (all-handin-counts-for-student s wd))))
       (students-with-any-graded-handin wd)))

; Path -> List-of Real
; List the mean grades for all students
(define (mean-grades wd)
  (map (lambda (s) (mean (filter (negate false?)
                                 (map student-score-points (student-scores s wd)))))
       (students-with-any-graded-handin wd)))

; Path -> Real
; Correlation between mean handin counts and mean grades (over all students)
(define (mean-handin-count-mean-grade-correlation wd)
  (correlation (mean-grades wd) (mean-handin-counts wd)))

;(require plot)
;(let ([wd "../../LocalPathForAllHandins/production"])
;  (plot (points (map vector (mean-handin-counts wd) (mean-grades wd)))))

; Other utilities
; ===============

; Display the mean number of handins for each homework
(define (display-mean-handin-numbers wd)
  (for ([hw (homework-folders wd)])
    (display (string-append (real->decimal-string (mean (list-numbers-of-handins wd (build-path wd hw)))) "\n"))))
