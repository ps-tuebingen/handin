#lang racket/base

(require racket/list
         racket/function
         racket/bool)
(provide (all-defined-out))

;; A GradingTable is the result of reading a `grade.rktd`-like file.
;; A ValidGradingTable is a list of entries, represented as two-element lists. For each entry:
;;   1. Either the first element is symbol 'grading-finished and the second is a boolean.
;;   2. Or the first element is a string and the second is a number.
;; Moreover, exactly one 'grading-finished entry must be present.
;; (We currently don't require that entry to be the first; requiring that would simplify the code a lot).
;; A FinishedGradingTable is ValidGradingTable where the grading-finished entry is true.
;;
;; Tutors should ensure that grading tables are only finished (according to the above definition of FinishedGradingTable)
;; when they're sure of the grading enough to show it to students.
;;

;; Deserialized a grade.rktd file.
; Filename -> GradingTable or #f
;; Input: a filename
(define (read-grading-scheme filename)
  (and (file-exists? filename)
       (with-handlers ([exn:fail? (const #f)])
         (call-with-input-file* filename
           (λ (input-port)
             ; Workaround Racket bug https://github.com/racket/racket/issues/1114
             (when (equal? (peek-char input-port) #\uFEFF)
               (read-char input-port))

             (read input-port))))))

(define (grading-finished-entry? entry)
  (and (eq? (first entry) 'grading-finished)
       (boolean? (second entry))))

(define (single-well-formed-grading-finished-entry? entries)
  (= 1 (length (filter grading-finished-entry? entries))))

; Any -> Boolean
; Tests whether entries is a ValidGradingTable.
(define (valid-grading-scheme? entries)
  (and (list? entries)
       (for/and ([entry (in-list entries)])
         (and (list? entry)
              (equal? (length entry) 2)
              (or (and (string? (first entry))
                       (number? (second entry)))
                  (grading-finished-entry? entry))
              (single-well-formed-grading-finished-entry? entries)))))

; GradingTable -> Bool
(define (erroneous-grading-scheme? entries)
  (not (valid-grading-scheme? entries)))

; ValidGradingTable -> Boolean
;; check whether something is a filled grading scheme marked as finished
;; Output: a boolean, #t iff the input grading table is a FinishedGradingTable.
(define (finished-grading-scheme? entries)
  (and (valid-grading-scheme? entries)
       (for/or ([entry (in-list entries)])
         (and (symbol? (first entry))
              (symbol=? (first entry) 'grading-finished)
              (second entry)))))

; FinishedGradingTable -> XExpr
;; convert filled grading scheme to definition list xexpr
(define (format-grading-table entries)
  (if (list? entries)
    `((table ([class "grading-scheme"])
       ,@(for/list
                   ([entry (in-list entries)]
                    #:when (string? (first entry)))
                    `(tr (td ,(number->string (second entry))) (td ,(first entry))))))
    `()))

; FinishedGradingTable -> Grade
;; compute total grade based on filled grading scheme
(define (grading-scheme-total entries)
  (for/sum ([entry (in-list entries)]
            #:when (string? (first entry)))
    (second entry)))

;; compute total grade from filename of the .rktd file
;; Input: filename
;; Output: a total grade, or #f in case of any errors.
(define (filename->grading-scheme-total filename)
  (define entries (read-grading-scheme filename))
  (and entries
       (finished-grading-scheme? entries)
       (grading-scheme-total entries)))

(module+ test
  (require rackunit)
  (check-false (finished-grading-scheme?
                 '([grading-finished #false]
                   ["TASK-1-A got the right result" TASK-1-A-CORRECT-RES]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES]
                   ["TASK-1-C got the right result" TASK-1-C-CORRECT-RES]
                   ["TASK-1-D got the right result" TASK-1-D-CORRECT-RES]
                   ["TASK-1-E got the right result" TASK-1-E-CORRECT-RES]
                   ["TASK-2-A got the right result" TASK-1-E-CORRECT-RES])))
  (check-false  (finished-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES])))
  (check-true   (finished-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" 1])))
  (check-true   (valid-grading-scheme?
                 '([grading-finished #f]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" 1])))
  (check-false  (finished-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ; The second `42` is outside of the string!
                   ["TASK-2-E (2pt) : Genauer gesagt, hätte 42 ein String sein müssen. Also als "42" geschrieben werden." 1])))
  (check-false  (valid-grading-scheme?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" 1]
                   [grading-finished #t])))
  (check-false  (valid-grading-scheme?
                 '(["TASK-1-A got the right result" 0]
                   ["TASK-1-B got the right result" 1]))))
