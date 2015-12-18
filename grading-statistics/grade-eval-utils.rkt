#lang racket/base

(provide (all-defined-out))
(require racket/list)
(require unstable/list)
(require math/statistics)

(require "../handin-server/format-grade.rkt")
(require "users.rkt")

(define GRADE-FILENAME "grade.rktd")
(define GRADE-MAX-FILENAME "grade-max.rktd")
(define DIRECTORY-SEARCH-DEPTH-LIMIT 2)


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

; (List-of Point) -> (List-of (Cons Bucket Number))
(define (grade-histogram ps)
  (map (lambda (xs) (cons (bucket-name (points->bucket (first xs))) (length xs)))
       (sort (group-by points->bucket ps) (lambda (x y) (< (first x) (first y))))))

(define (normalized-grade-histogram ps)
  (let* ((h (grade-histogram ps))
         (count (foldl (lambda (y x) (+ x (cdr y))) 0 h)))
    (map (lambda (x) (cons (car x) (* 100 (/ (cdr x) count)))) h)))

; (List-of FinishedGradingTable) -> (List-Of (Cons Bucket Number))
(define (grade-histogram-for-gt gs)
  (grade-histogram (map grading-table-total gs)))

(define (normalized-grade-histogram-for-gt gs)
  (normalized-grade-histogram (map grading-table-total gs)))


; Path -> (union Path 'up 'same)
; extract the immediate directory or file name from a path
(define (basename dir-or-file)
  (let-values ([(_1 filename _2) (split-path dir-or-file)])
    filename))

; Path -> (List-Of Path)
(define (find-all-grade-files dir max-depth)
  (if (= max-depth 0)
      (list (build-path dir GRADE-FILENAME))
      (apply append
             (map (lambda (p) (find-all-grade-files p (- max-depth 1)))
                  (filter directory-exists? (directory-list dir #:build? #t))))))

; A GradingRecord pairs a GradingTable and a StudentName and a Tutor
(define-struct grading-record (table name tutor))


; A FinishedGradingRecord is a GradingRecord whose GradingTable is a FinishedGradingTable.

; Path -> (List-of GradingTable)
(define (all-grading-tables wd)
  (map read-grading-table (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))

; Path -> (List-of GradingRecord)
(define (all-grading-tables* wd)
  (map (λ (p)
         (grading-record (read-grading-table p) (get-user-name-from-path p) (get-tutor-name-from-path p)))
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

; Path -> (List-of UnfinishedGradingRecord)
(define (all-erroneous-grading-tables* wd)
  (filter (λ (gr)
            (erroneous-grading-table? (grading-record-table gr)))
          (all-grading-tables* wd)))

; Path -> (List-of UnfinishedGradingRecord)
(define (all-unfinished-grading-tables* wd)
  (filter (λ (gr)
            (not (finished-grading-table? (grading-record-table gr))))
          (all-grading-tables* wd)))

; Path -> (List-of Path)
(define (all-unfinished-grading-tables wd)
  (filter (lambda (p) (not (finished-grading-table? (read-grading-table p))))
          (find-all-grade-files wd DIRECTORY-SEARCH-DEPTH-LIMIT)))


; Path -> StudentName
(define (get-user-name-from-path p)
  (let* ((path-components (explode-path p))
         (numOfPC (length path-components)))
    (if (> numOfPC 1)
        (path-element->string (list-ref path-components (- numOfPC 2)))
        "unknown")))

; Path -> TutorName
(define (get-tutor-name-from-path p)
  (let* ((path-components (explode-path p))
         (numOfPC (length path-components)))
    (if (> numOfPC 2)
        (list-ref path-components (- numOfPC 3))
        p)))

; (list-of GradingRecord) (Grading-Record -> String) String -> ()
; displays list of grading records, grouped by tutor
(define (display-grading-tables gts showgr title)
  (when (> (length gts) 0)
      (begin
        (display title)
        (newline)
        (display "---")
        (newline)
        (display (format "Total number: ~a\n" (length gts)))
        (for ([tgts (group-by grading-record-tutor gts)]
              #:when (not (empty? tgts)))
          (begin
            (display (format "Tutor: ~a\n" (grading-record-tutor (first tgts))))
            (for ([gt tgts])
              (display (showgr gt)))
            (newline) (newline))))))


(define (list-unfinished wd)
  (let ((unfinished (all-unfinished-grading-tables* wd)))
    (display-grading-tables unfinished
                            (lambda (u) (format "~a " (grading-record-name u)))
                            "Unfinished grading tables:")))

(define (list-grades wd)
  (define finished (all-finished-grading-tables* wd))
  (display (format "Total number of finished grading tables: ~a\n" (length finished)))
  (for ([g finished])
    (display (format "~a: ~a\n"
                     (grading-record-name g)
                     (grading-table-total (grading-record-table g))))))



(define (list-erroneous wd)
  (let* ((all (all-grading-tables* wd))
        (non-existing-grading-file (filter (lambda (gr) (eq? (grading-record-table gr) #false)) all))
        (existing-grading-file (filter (lambda (gr) (not (eq? (grading-record-table gr) #false))) all))
        (erroneous (filter (lambda (gr) (erroneous-grading-table? (grading-record-table gr))) existing-grading-file))
        (morethan100points (filter (lambda (gs) (> (grading-table-total (grading-record-table gs)) 100)) (all-finished-grading-tables* wd))))
    (begin
      (display-grading-tables non-existing-grading-file
                              (lambda (p) (format "~a " (grading-record-name p)))
                              "Datei grade.rktd existiert nicht oder kann nicht geladen werden")
      (display-grading-tables erroneous
                              (lambda (p) (format "~a " (grading-record-name p)))
                              "Grading ist noch nicht fertig oder Fehler im Format")
;      (display-grading-tables morethan100points
;                              (lambda (p) (format "~a " (grading-record-name p)))
;                              "Grading tables with more than 100 points: ")
  )))


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
      (list-erroneous wd)
      (display-grading-tables wrongNoOfGrades
                              (lambda (p) (format "~a " (grading-record-name p)))
                              "Anzahl der benoteten Aufgaben stimmt nicht")
      (display-grading-tables tooManyPointsOnTask
                              (lambda (p) (format "~a " (grading-record-name p)))
                              "Enthält Bewertungen mit zu vielen Punkten"))))
;      (display (format "Total number of grading tables with incorrect number of grades: ~a\n" (length wrongNoOfGrades)))
;      (for ([p wrongNoOfGrades])
;        (display (format "~a " (grading-record-name p))))
;      (newline)
;      (display (format "Total number of grading tables with too many points on some tasks: ~a\n" (length tooManyPointsOnTask)))
;      (for ([p tooManyPointsOnTask])
;        (display (format "~a " (grading-record-name p))))
;      (newline))))


(define (scores wd) (map grading-table-total (all-finished-grading-tables wd)))

(define (stats wd)
  (define n-scores (length (scores wd)))
  (display (format "Number of finished grade files: ~a\n" n-scores))
  (when (> n-scores 0)
    (display (format "Mean score: ~a\n" (mean (scores wd))))
    (display (format "Median score: ~a\n" (median < (scores wd))))))

(define (stats-by-studiengang wd)
  (let* ((grading-records (all-finished-grading-tables* wd))
         (number-of-records (length grading-records))
         (sorted-grading-records (sort
                                  grading-records
                                  (lambda (gt1 gt2)
                                            (>
                                              (grading-table-total (grading-record-table gt1))
                                              (grading-table-total (grading-record-table gt2))))))
         ; index-and-studiengang has the form  '((Kognitionswissenschaft . 17) (Bioinformatik . 16) ...)
         (index-and-studiengang (cdr (foldr (lambda (x xs) (cons (+ (car xs) 1)
                                                            (cons
                                                             (cons (hash-ref user->studiengang (grading-record-name x))
                                                                   (car xs))
                                                             (cdr xs))))
                                            (cons 0 empty)
                                            sorted-grading-records)))
         (grouped-by-studiengang (group-by (lambda (x) (car x)) index-and-studiengang))
         (median-per-studiengang (map (lambda (gbs) (cons (car (first gbs)) (* 100 (/ (median < (map cdr gbs )) number-of-records)))) grouped-by-studiengang)))
    
    
         
    (for [(q median-per-studiengang)]
         (display (format "Studiengang ~a : Median Prozentpercentil: ~a %\n" (car q) (real->decimal-string (cdr q)))))))


(define (histo wd)
  (for [( q (normalized-grade-histogram-for-gt (all-finished-grading-tables wd)))]
    (display (format "Point range ~a : ~a %\n" (car q) (real->decimal-string (cdr q))))))

(define (histo-by-studiengang wd)
  (let* ((grading-records (all-finished-grading-tables* wd))
         (grading-records-by-studiengang
           (group-by (lambda (gr) (hash-ref user->studiengang (grading-record-name gr) "unknown")) grading-records)))
    (for [(sg grading-records-by-studiengang)]
         (begin
           ;(display (format "~a" (map grading-record-name grading-records)))
           (display (format "Studiengang: ~a , Anzahl: ~a\n "
                            (hash-ref user->studiengang (grading-record-name (first sg)) "unknown")
                            (length sg)))
           (for [(q (normalized-grade-histogram-for-gt (map grading-record-table sg)))]
                (display (format "Point range ~a : ~a %\n" (car q) (real->decimal-string (cdr q)))))))))

(define (means-list wd)
  (for [(f (directory-list wd))]
    (let ([directory (build-path wd f)])
      (when (and (directory-exists? directory) (> (length (scores directory)) 0))
        (display (format "~a : mean score : ~a %\n"
                         f
                         (real->decimal-string (mean (scores directory)))))))))

(define (student-scores s wd)
  (for [(f (directory-list wd))]
    (define is-homework-folder (char-numeric? (first (string->list (path->string f)))))
    (let* ([exercise-directory (build-path wd f)]
           [student-directory (build-path exercise-directory s)]
           [grade-files (find-all-grade-files student-directory 0)])
      (when (and (directory-exists? exercise-directory) is-homework-folder)
        (if (directory-exists? student-directory)
            (when (> (length grade-files) 0)
              (let ([grading-table (read-grading-table (car grade-files))])
                (if (and (valid-grading-table? grading-table)
                         (finished-grading-table? grading-table))
                    (display (format "~a : score : ~a %\n"
                                     f
                                     (grading-table-total (read-grading-table (car grade-files)))))
                    (display (format "~a : unfinished or invalid grading table\n" f)))))
            (display (format "~a : no homework handed in\n" f)))))))

(define (exercise-score i gt)
  (second (list-ref gt i)))

(define (normalized-exercise-score i gt maxt)
  (let ([max-score (third (list-ref maxt i))])
    (/ (exercise-score i gt) max-score)))

; Real -> Real
(define (percentify x)
  (* 100 x))

(define (means-per-exercise wd)
  (define max-template (read-grading-table (build-path wd GRADE-MAX-FILENAME)))
  (define grading-records (all-finished-grading-tables wd))
  (define (max-score i) (third (list-ref max-template i)))
  (for ([i (range 1 (length max-template))])
    (let ([scores (map (lambda (gt) (exercise-score i gt)) grading-records)])
      (if (empty? scores)
          (display (format "No grading for exercise #~a\n" i))
          (display (format "Mean for exercise #~a : ~a %\n"
                           i
                           (real->decimal-string
                            (percentify
                             (mean
                              (map
                               (lambda (gt) (normalized-exercise-score i gt max-template))
                               grading-records))))))))))

(define (histo-for-exercise i wd)
  (define max-template (read-grading-table (build-path wd GRADE-MAX-FILENAME)))
  (for [(q (normalized-grade-histogram (map (lambda (gt) (percentify (normalized-exercise-score i gt max-template)))
                                            (all-finished-grading-tables wd))))]
    (display (format "Point range ~a : ~a %\n" (car q) (real->decimal-string (cdr q))))))

(define (parse-schema s)
  (if (and
       (list? s)
       (for/and [(m s)] (number? m)))
      s
      (error (format "Cannot parse grade schema, must be list of numbers: ~a" s))))
