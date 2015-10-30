#lang racket/base

(require racket/list
         racket/bool)
(provide (all-defined-out))

;; A grading table is the result of reading a `grade.rktd`-like file.
;; A complete grading table is a list of entries, represented as two-element lists. For each entry.
;; 1. Either the first element is symbol 'grading-finished and the second is #true.
;; 2. Or the first element is a string and the second is a number.
;; The 'grading-finished entry must be present.
;;
;; Tutors should ensure that grading tables are only complete (according to the above definition)
;; when they're sure of the grading enough to show it to students.
;;

;; check whether something is a filled grading scheme marked as finished
;; Input: a grading table
;; Output: a boolean, #t iff the input grading table is valid.
(define (finished-grading-scheme? entries)
  (and (list? entries)
       (for/and ([entry (in-list entries)])
         (and (list? entry)
              (or (and (string? (first entry))
                       (number? (second entry)))
                  (and (symbol? (first entry))
                       (symbol=? (first entry) 'grading-finished)
                       (second entry)))))
       (for/or ([entry (in-list entries)])
         (and (list? entry)
              (symbol? (first entry))
              (symbol=? (first entry) 'grading-finished)))))

;; convert filled grading scheme to definition list xexpr
;; Input: a valid grading table
;; Output: an xexpr
(define (format-grade-details entries)
  (if (list? entries)
    `((table ([class "grading-scheme"])
       ,@(for/list
                   ([entry (in-list entries)]
                    #:when (string? (first entry)))
                    `(tr (td ,(number->string (second entry))) (td ,(first entry))))))
    `()))

;; compute total grade based on filled grading scheme
;; Input: a valid grading table
;; Output: a grade
(define (grading-scheme-total entries)
  (for/sum ([entry (in-list entries)]
            #:when (string? (first entry)))
    (second entry)))


(module+ test
  (require rackunit)
  (check-equal? #f
                (finished-grading-scheme?
                 '([grading-finished #false]
                   ["TASK-1-A got the right result" TASK-1-A-CORRECT-RES]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES]
                   ["TASK-1-C got the right result" TASK-1-C-CORRECT-RES]
                   ["TASK-1-D got the right result" TASK-1-D-CORRECT-RES]
                   ["TASK-1-E got the right result" TASK-1-E-CORRECT-RES]
                   ["TASK-2-A got the right result" TASK-1-E-CORRECT-RES])))
  (check-equal? #f
                (finished-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES])))
  (check-equal? #t
                (finished-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" 1])))
  )
