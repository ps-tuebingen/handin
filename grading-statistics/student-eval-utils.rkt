#lang racket/base

(provide (all-defined-out))
(require racket/list)
(require racket/math)
(require racket/function)
(require unstable/list)
(require racket/bool)
(require math/statistics)

(require "../handin-server/format-grade.rkt")
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
  (define (is-homework-folder? p)
    (and (directory-exists? p)
         (char-numeric? (first (string->list
                                (call-with-values
                                 (lambda () (split-path p))
                                 (lambda (b n d) (path->string n))))))))
  (filter is-homework-folder? (directory-list wd #:build? #t)))

; String Path -> List-of StudentScore
; The list of all scores for the given student (over all homework subdirectories in the wd)
(define (student-scores s wd)
  (filter (negate void?)
          (for/list ([f (directory-list wd)])
            (define is-homework-folder (char-numeric? (first (string->list (path->string f)))))
            (let* ([exercise-directory (build-path wd f)]
                   [student-directory (build-path exercise-directory s)])
              (when (and (directory-exists? exercise-directory) is-homework-folder)
                (retrieve-student-score student-directory))))))

(define (display-student-scores s wd)
  (for ([scr (student-scores s wd)])
    (let ([exercise-name (student-score-path scr)])
    (if (student-score-handin? scr)
        (if (student-score-points scr)
            (display (format "~a : score : ~a %\n" exercise-name (student-score-points scr)))
            (display (format "~a : unfinished or invalid grading table\n" exercise-name)))
        (display (format "~a : no homework handed in\n" exercise-name))))))

; Path (U #f Points) (U #f Points) -> List-of String
; List all students that have a graded handin, respective to the given homework directory hwd
; With the optional arguments min and max, one can exclude students which have average (over all hw)
; grades below or above a certain number of points
(define (students-with-graded-handins wd hwd [min 0] [max +inf.0])
  (filter (lambda (s) (and
                       (student-score-points (retrieve-student-score (build-path hwd s)))
                       (let ([m (mean (filter (negate false?) (map student-score-points (student-scores s wd))))])
                         (and (> m min) (< m max)))))
          (remove-duplicates (map get-user-name-from-path (find-all-grade-files hwd 1)))))

; Path -> List-of String
; List all students that have a graded handin for any of the homeworks
(define (students-with-any-graded-handin wd)
  (remove-duplicates (append-map (lambda (hwd) (students-with-graded-handins wd hwd))
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
; How often the student handed in for the respective homework (as given by wd), false when there is no directory for this student
(define (number-of-handins s wd)
  (let ([student-directory (build-path wd s)])
    (if (directory-exists? student-directory)
        (length (filter (handin-dir? student-directory) (directory-list student-directory)))
        #f)))

; Approach A:
; -----------
; fix the homework, then consider how scores and #handins correlate (calculated over all students)

; Path Path -> List-of Natural
; Lists the numbers of handins for the respective homework (as given by hwd) for all students which have handed in this hw
; The optional arguments min and max are the same as for student-with-graded-handins
; and allow to exclude certain students.
(define (list-numbers-of-handins wd hwd [min 0] [max +inf.0])
  (map (lambda (s) (number-of-handins s hwd))
       (students-with-graded-handins wd hwd min max)))

; Path -> List-of Points
; Lists the scores for the respective homework (as given by hwd) for all students
; The students' order is identical to that returned by list-numbers-of-handins.
; The optional arguments min and max are the same as for student-with-graded-handins
; and allow to exclude certain students.
(define (list-points wd hwd [min 0] [max +inf.0])
  (map (lambda (s) (student-score-points (retrieve-student-score (build-path hwd s))))
       (students-with-graded-handins wd hwd min max)))

; Path -> Real
; Correlation between number of handins and grades for the homework given by hwd
(define (handin-count-grade-correlation wd hwd [min 0] [max +inf.0])
  (correlation (list-points wd hwd min max) (list-numbers-of-handins wd hwd min max)))

; Approach B:
; -----------
; consider the mean # of handins and mean score of each student and how these means correlate

; String Path -> List-of Natural
; Lists all handin counts for the given student
(define (all-handin-counts-for-student s wd)
  (filter (negate void?)
          (for/list ([f (directory-list wd)])
            (define is-homework-folder (char-numeric? (first (string->list (path->string f)))))
            (let ([exercise-directory (build-path wd f)])
              (when (and (directory-exists? exercise-directory) is-homework-folder)
                (number-of-handins s exercise-directory))))))
; TODO: extract code shared with student-scores into some abstraction(s)

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
