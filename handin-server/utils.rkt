#lang racket/base

(require racket/class racket/gui/base racket/pretty
         (prefix-in pc: mzlib/pconvert)
         ; Only import identifiers we want to reexport below
         (only-in "private/main-params.rkt" timeout-control get-user-assignment-directory get-assignment-name get-client-plugin-version)
         "private/run-status.rkt"
         "private/config.rkt"
         "private/logger.rkt"
         "sandbox.rkt")

(provide (all-from-out "sandbox.rkt")
         (all-from-out "private/main-params.rkt")

         get-conf
         log-line
         server-dir

         unpack-submission

         make-evaluator/submission
         evaluate-all
         evaluate-submission

         call-with-evaluator
         call-with-evaluator/submission
         reraise-exn-as-submission-problem
         set-run-status
         message
         current-value-printer

         check-proc
         check-defined
         look-for-tests
         user-construct
         test-history-enabled

         list-abbreviation-enabled)

(define (unpack-submission str)
  (let* ([base (make-object editor-stream-in-bytes-base% str)]
         [stream (make-object editor-stream-in% base)]
         [definitions-text (make-object text%)]
         [interactions-text (make-object text%)])
    (read-editor-version stream base #t)
    (read-editor-global-header stream)
    (send definitions-text read-from-file stream)
    (send interactions-text read-from-file stream)
    (read-editor-global-footer stream)
    (values definitions-text interactions-text)))

;; Execution ----------------------------------------

(define (make-evaluator* lang reqs allowed-requires inp)
  (reraise-exn-as-submission-problem
   (lambda ()
     (if (and (list? lang) (= 2 (length lang)) (eq? 'module (car lang)))
         (make-module-evaluator inp #:language (cadr lang) #:allow-read reqs
                                #:allow-syntactic-requires allowed-requires
                                #:readers '((lib "htdp-beginner-reader.ss" "lang")
                                            (lib "htdp-beginner-abbr-reader.ss" "lang")
                                            (lib "htdp-intermediate-reader.ss" "lang")
                                            (lib "htdp-intermediate-lambda-reader.ss" "lang")
                                            (lib "htdp-advanced-reader.ss" "lang")))
         (make-evaluator lang inp #:requires reqs
                         #:allow-syntactic-requires allowed-requires)))))

(define (open-input-text-editor/lines str)
  (let ([inp (open-input-text-editor str)])
    (port-count-lines! inp) inp))

(define (make-evaluator/submission language requires str)
  (let-values ([(defs interacts) (unpack-submission str)])
    (make-evaluator* language requires (open-input-text-editor defs))))

(define (evaluate-all source port eval)
  (let loop ()
    (let ([expr (parameterize ([read-case-sensitive #t]
                               [read-decimal-as-inexact #f])
                  (read-syntax source port))])
      (unless (eof-object? expr)
        (eval expr)
        (loop)))))

(define (evaluate-submission str eval)
  (let-values ([(defs interacts) (unpack-submission str)])
    (evaluate-all 'handin (open-input-text-editor/lines defs) eval)))

(define (reraise-exn-as-submission-problem thunk)
  (with-handlers ([void (lambda (exn)
                          (error (if (exn? exn)
                                   (exn-message exn)
                                   (format "exception: ~.s" exn))))])
    (thunk)))

;; ----------------------------------------
;;  Auto-test utils

(define (check-defined e id)
  (with-handlers ([exn:fail:syntax? void]
                  [exn:fail:contract:variable?
                   (lambda (x)
                     (error
                      (format
                       "\"~a\" is not defined, but it must be defined for handin"
                       (exn:fail:contract:variable-id x))))])
    (e #`(#,namespace-variable-value '#,id #t))))

(define test-history-enabled (make-parameter #f))
(define test-history (make-parameter null))

(define (format-history one-test)
  (if (test-history-enabled)
    (format "(begin~a)"
            (apply string-append (map (lambda (s) (format " ~a" s))
                                      (reverse (test-history)))))
    one-test))

(define (check-proc e result equal? f . args)
  (let ([test (format "(~a~a)" f
                      (apply string-append
                             (map (lambda (a) (format " ~e" a)) args)))])
    (when (test-history-enabled)
      (test-history (cons test (test-history))))
    (set-run-status (format "running instructor-supplied test ~a"
                            (format-history test)))
    (let-values ([(ok? val)
                  (with-handlers ([void
                                   (lambda (x)
                                     (error
                                      (format "instructor-supplied test ~a failed with an error: ~.s"
                                              (format-history test)
                                              (exn-message x))))])
                    (let ([val (e `(,f ,@(map value-converter args)))])
                      (values (or (eq? 'anything result)
                                  (equal? val result))
                              val)))])
      (unless ok?
        (error
         (format "instructor-supplied test ~a should have produced ~e, instead produced ~e"
                 (format-history test) result val)))
      val)))

(define (user-construct e func . args)
  (apply check-proc e func 'anything eq? args))

(define (look-for-tests t name count)
  (let ([p (open-input-text-editor/lines t)])
    (let loop ([found 0])
      (let ([e (read p)])
        (if (eof-object? e)
          (when (found . < . count)
            (error (format "found ~a test~a for ~a, need at least ~a test~a"
                           found
                           (if (= found 1) "" "s")
                           name
                           count
                           (if (= count 1) "" "s"))))
          (loop (+ found (if (and (pair? e) (eq? (car e) name)) 1 0))))))))

(define list-abbreviation-enabled (make-parameter #f))

(define (value-converter v)
  (parameterize ([pc:booleans-as-true/false #t]
                 [pc:abbreviate-cons-as-list (list-abbreviation-enabled)]
                 [pc:constructor-style-printing #t])
    (pc:print-convert v)))

(define (default-value-printer v)
  (parameterize ([pretty-print-show-inexactness #t]
                 [pretty-print-.-symbol-without-bars #t]
                 [pretty-print-exact-as-decimal #t]
                 [pretty-print-columns 'infinity]
                 [read-case-sensitive #t])
    (let ([p (open-output-string)])
      (pretty-print (value-converter v) p)
      (regexp-replace #rx"\n$" (get-output-string p) ""))))
(define current-value-printer (make-parameter default-value-printer))

(define (call-with-evaluator lang requires program-port
                             #:allowed-requires [allowed-requires #f]
                             go)
  (parameterize ([error-value->string-handler (lambda (v s)
                                                ((current-value-printer) v))]
                 [list-abbreviation-enabled
                  (not (or (equal? lang '(special beginner))
                           (equal? lang '(module (lib "htdp-beginner.ss" "lang")))))])
    (reraise-exn-as-submission-problem
     (lambda ()
       (let ([e (make-evaluator* lang requires allowed-requires program-port)])
         (set-run-status "executing your code")
         (go e))))))

(define (call-with-evaluator/submission lang requires str
                                        #:allowed-requires [allowed-requires #f]
                                        go)
  (let-values ([(defs interacts) (unpack-submission str)])
    (call-with-evaluator lang requires (open-input-text-editor defs)
                         #:allowed-requires allowed-requires
                         go)))
