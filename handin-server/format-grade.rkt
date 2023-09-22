#lang racket/base

(require racket/list
         racket/function
         racket/string
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
(define (read-grading-table filename)
  (and (file-exists? filename)
       (with-handlers ([exn:fail? (const #f)])
         (call-with-input-file* filename
           (λ (input-port)
             ; Workaround Racket bug https://github.com/racket/racket/issues/1114
             (when (equal? (peek-char input-port) #\uFEFF)
               (read-char input-port))
             ; Skip Racket header (lines that start with ; or #), if present
             (letrec ((skipheader (lambda ()
                                    (when (or (equal? (peek-char input-port) #\;)
                                              (equal? (peek-char input-port) #\#))
                                      (begin
                                        (read-line input-port)
                                        (skipheader))))))
               (skipheader))
             
             (read input-port))))))

(define (grading-finished-entry? entry)
  (and (symbol? (first entry))
       (symbol=? (first entry) 'grading-finished)
       (boolean? (second entry))))

(define (feedback-entry? entry)
  (and (string? (first entry))
       (string-prefix? (first entry) "Feedback")
       (string? (second entry))))

(define (score-entry? entry)
  (and (string? (first entry))
       (string-prefix? (first entry) "Bewertung")
       (number? (second entry))))

(define (bullet-entry? entry)
  (and (string? (first entry))
       (symbol? (second entry))
       (let ([bullet (second entry)])
         (or (symbol=? bullet '-) (symbol=? bullet 'o) (symbol=? bullet '+)))))

(define (single-well-formed-grading-finished-entry? entries)
  (= 1 (length (filter grading-finished-entry? entries))))

; Any -> Boolean
; Tests whether entries is a ValidGradingTable.
(define (valid-grading-table? entries)
  (and (list? entries)
       (for/and ([entry (in-list entries)])
         (and (list? entry)
              (equal? (length entry) 2)
              (or (score-entry? entry)
                  (feedback-entry? entry)
                  (bullet-entry? entry)
                  (grading-finished-entry? entry))
              (single-well-formed-grading-finished-entry? entries)))))

; GradingTable -> Bool
(define (erroneous-grading-table? entries)
  (not (valid-grading-table? entries)))

; ValidGradingTable -> Boolean
;; check whether something is a filled grading table marked as finished
;; Output: a boolean, #t iff the input grading table is a FinishedGradingTable.
(define (finished-grading-table? entries)
  (and (valid-grading-table? entries)
       (for/or ([entry (in-list entries)])
         (and (symbol? (first entry))
              (symbol=? (first entry) 'grading-finished)
              (second entry)))))

; FinishedGradingTable -> XExpr
;; convert filled grading table to definition list xexpr
(define (format-grading-table entries)
  (if (list? entries)
    `((table ([class "grading-table"])
       ,@(for/list
                   ([entry (in-list entries)]
                    #:when (string? (first entry)))
                    `(tr (td ,(let ([rating (second entry)])
                                (cond
                                  [(symbol? rating) (symbol->string rating)]
                                  [(number? rating) (number->string rating)]
                                  [else rating])))
                         (td ,(if (string-prefix? (first entry) "Bewertung")
                                `(b ,(first entry))
                                (first entry)))))))
    `()))

; FinishedGradingTable -> Grade
;; compute total grade based on filled grading table
(define (grading-table-total entries)
  (for/sum ([entry (in-list entries)]
            #:when (and (string? (first entry))
                        (string-prefix? (first entry) "Bewertung")))
    (second entry)))

;; compute total grade from filename of the .rktd file
;; Input: filename
;; Output: a total grade, or #f in case of any errors.
(define (filename->grading-table-total filename)
  (define entries (read-grading-table filename))
  (and entries
       (finished-grading-table? entries)
       (grading-table-total entries)))

(module+ test
  (require rackunit)
  (check-false (finished-grading-table?
                 '([grading-finished #false]
                   ["TASK-1-A got the right result" TASK-1-A-CORRECT-RES]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES]
                   ["TASK-1-C got the right result" TASK-1-C-CORRECT-RES]
                   ["TASK-1-D got the right result" TASK-1-D-CORRECT-RES]
                   ["TASK-1-E got the right result" TASK-1-E-CORRECT-RES]
                   ["TASK-2-A got the right result" TASK-1-E-CORRECT-RES])))
  (check-false  (finished-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" TASK-1-B-CORRECT-RES])))
  (check-true   (finished-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '-]
                   ["TASK-1-B got the right result" 'o])))
  (check-true   (valid-grading-table?
                 '([grading-finished #f]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-])))
  (check-false  (finished-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ; The second `42` is outside of the string!
                   ["TASK-2-E: Genauer gesagt, hätte 42 ein String sein müssen. Also als "42" geschrieben werden." '-])))
  (check-false  (valid-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" 'o]
                   ["TASK-1-B got the right result" '+]
                   [grading-finished #t])))
  (check-false  (valid-grading-table?
                 '(["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-])))
  (check-true   (valid-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Bewertung Aufgabe 1" 2])))
  (check-false   (valid-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Bewertung Aufgabe 1" nicht-automatisch-bewertet])))
  (check-true   (valid-grading-table?
                 '([grading-finished #f]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Bewertung Aufgabe 1" nicht-automatisch-bewertet])))
  (check-true   (valid-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Feedback Aufgabe 1: Bei TASK-1-B gab es Probleme." ""])))
  (check-false   (valid-grading-table?
                 '([grading-finished #t]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Feedback Aufgabe 1" nicht-automatisch-bewertet])))
  (check-true   (valid-grading-table?
                 '([grading-finished #f]
                   ["TASK-1-A got the right result" '+]
                   ["TASK-1-B got the right result" '-]
                   ["Feedback Aufgabe 1" nicht-automatisch-bewertet]))))
