#lang racket

(require "../handin-server/format-grade.rkt")

(define GRADE-MAX-FILENAME "grade-max.rktd")

(define LANG-DEF-FILENAME "lang-def.rkt")

(define HEADER "#lang racket\n; Use this language definition to verify filled-in grade files\n; To check such a file, use\n;  #lang s-exp \"lang-def.rkt\"\n; in the header.\n")

(define PROVIDE '(provide (except-out (all-from-out racket)
                                      #%module-begin)
                          (rename-out [module-begin #%module-begin])))

; Integer -> Symbol
(define (generate-points-placeholder i)
  (string->symbol (string-append "p" (number->string i))))

; (List-of String) (List-of Integer) -> SExp
;; autogenerate language definition according to the given exercise descriptions and max. points per exercise
(define (generate-lang-def-rule ds ps)
  (let ([ps-range (range (length ps))])
    `(define-syntax-rule
       (module-begin ([gf bool]
                      ,@(map (lambda (d i) `[,d ,(generate-points-placeholder i)]) ds ps-range)))
       (#%module-begin
        (define check-format
          (cond
            [(not (symbol=? 'gf 'grading-finished)) (error "Formatfehler: Bezeichner muss 'grading-finished' sein")]
            [(not (boolean? bool)) (error "Angabe für grading-finished muss boolean sein")]))
        (define correct-max-points?
          (and
           ,@(map (lambda (m i) `(<= ,(generate-points-placeholder i) ,m)) ps ps-range)))
        (define check-max-points (if correct-max-points? #t (error "Höchstpunktzahl überschritten")))
        (define check (and check-format check-max-points))
        (provide check)))))

;; A MaxPointsTable is the result of reading a `grade-max.rktd`-like file.

; MaxPointsTable -> (List-of String)
(define (descriptions entries)
  (map first (cdr entries)))

; MaxPointsTable -> (List-of Integer)
(define (max-points entries)
  (map third (cdr entries)))

(let* ([entries (read-grading-table GRADE-MAX-FILENAME)] ; TODO: read from `<wd>/grade-max.rktd` (need to process wd taken from args)
       [rule (format "~s" (generate-lang-def-rule (descriptions entries) (max-points entries)))]
       [output (string-append HEADER (format "~s" PROVIDE) "\n" rule)])
  (display-to-file output LANG-DEF-FILENAME))
