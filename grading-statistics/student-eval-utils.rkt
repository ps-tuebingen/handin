#lang racket/base

(provide (all-defined-out))
(require racket/list)
(require racket/function)
(require unstable/list)

(require "../handin-server/format-grade.rkt")
(require "grade-eval-utils.rkt")

; Individual student-based grade evaluation
; =========================================

; A StudentScore consists of
; - a Path: the exercise folder.
; - a Bool: When the student handed something in it is #t, otherwise #f.
; - a U Points Bool: Only relevant when the second part is #t. In this case it is the points
; the student achieved - unless grading isn't finished/erroneous, then it is #f.
(define-struct student-score (path handin? points))

; String Path -> List-of StudentScore
(define (student-scores s wd)
  (filter (negate void?)
          (for/list ([f (directory-list wd)])
            (define is-homework-folder (char-numeric? (first (string->list (path->string f)))))
            (let* ([exercise-directory (build-path wd f)]
                   [student-directory (build-path exercise-directory s)]
                   [grade-files (find-all-grade-files student-directory 0)])
              (when (and (directory-exists? exercise-directory) is-homework-folder)
                (if (directory-exists? student-directory)
                    (if (> (length grade-files) 0)
                        (let ([grading-table (read-grading-table (car grade-files))])
                          (if (and (valid-grading-table? grading-table)
                                   (finished-grading-table? grading-table))
                              (make-student-score f #t (grading-table-total (read-grading-table (car grade-files))))
                              (make-student-score f #t #f)))
                        (make-student-score f #t #f))
                    (make-student-score f #f #f)))))))

(define (display-student-scores s wd)
  (for ([scr (student-scores s wd)])
    (let ([exercise-name (student-score-path scr)])
    (if (student-score-handin? scr)
        (if (student-score-points scr)
            (display (format "~a : score : ~a %\n" exercise-name (student-score-points scr)))
            (display (format "~a : unfinished or invalid grading table\n" exercise-name)))
        (display (format "~a : no homework handed in\n" exercise-name))))))

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
