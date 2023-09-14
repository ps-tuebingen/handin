#lang racket/base

(require syntax/moddep "logger.rkt")

;; this module provides provide/monitor and protect, designed
;; to guard evaluation of named forms using a single semaphore.
;; all defined or protected forms are guarded by the same semaphore.
;; provide/monitor provides a function, and names for its arguments.
;; protect is wrapped around an expression. 
(module mon racket/base
  (define sema (make-semaphore 1))
  (define-syntax-rule
    (provide/monitor (id x ...))
    (begin
      (define -id
        (let ([id (λ (x ...)
                    (call-with-semaphore
                     sema
                     (λ () (id x ...))))])
          id))
      (provide (rename-out [-id id]))))
  (define-syntax-rule
    (protect e)
    (call-with-semaphore sema (λ () e)))
  (provide provide/monitor protect))
(require (submod "." mon))

(module+ test
  (module m racket/base
    (require (submod ".." ".." mon))
    (define (f x) (* 2 (g x)))
    (define (g x) (+ x 1))
    (provide/monitor (f x))
    (provide/monitor (g x)))
  (require (submod "." m) rackunit)
  (check-equal? (g 2) 3)
  (check-equal? (f 11) 24))

(provide/monitor (reload-module modspec path))
(define (reload-module modspec path)
  ;; the path argument is not needed (could use resolve-module-path here), but
  ;; its always known when this function is called
  (let* ([name ((current-module-name-resolver) modspec #f #f #t)])
    (log-line "(re)loading module from ~a" modspec)
    (parameterize ([current-module-declare-name name]
                   [compile-enforce-module-constants #f])
      ;; only notify, it's fine to reset the file timer, since there's no point
      ;; in attempting to reload it yet again until it is edited.
      (with-handlers ([exn? (lambda (e)
                              (log-line "error, module not reloaded (~a)"
                                        (exn-message e)))])
        (namespace-require '(only racket module #%top-interaction))
        (load/use-compiled path)))))

;; pulls out a value from a module, reloading the module if its source file was
;; modified
(provide/monitor (auto-reload-value modspec valname))
(define module-times (make-hash))
(define (auto-reload-value modspec valname)
  (define path0 (resolve-module-path modspec #f))
  (define last  (hash-ref module-times path0 #f))
  (define-values (path cur)
    (let ([s (file-or-directory-modify-seconds path0 #f (lambda () #f))])
      (if s
        (values path0 s)
        (let* ([p (and (regexp-match? #rx#"[.]rkt$" (path->bytes path0))
                       (path-replace-suffix path0 #".ss"))]
               [s (and p (file-or-directory-modify-seconds p #f (lambda () #f)))])
          (if s (values p s) (values path0 +inf.0))))))
  (unless (equal? cur last)
    (hash-set! module-times path cur)
    (reload-module modspec path))
  (dynamic-require modspec valname))

(define poll-freq 2000.0) ; poll at most once every two seconds

;; pulls out a procedure from a module, and returns a wrapped procedure that
;; automatically reloads the module if the file was changed whenever the
;; procedure is used
(provide/monitor (auto-reload-procedure x y))
(define (auto-reload-procedure modspec procname)
  (let ([path (resolve-module-path modspec #f)] [date #f] [proc #f] [poll #f])
    (define (reload)
      (unless (and proc (< (- (current-inexact-milliseconds) poll) poll-freq))
        (set! poll (current-inexact-milliseconds))
        (let ([cur (file-or-directory-modify-seconds path)])
          (unless (equal? cur date)
            (set! date cur)
            (reload-module modspec path)
            (set! proc (dynamic-require modspec procname))))))
    (reload)
    (lambda xs (protect (reload)) (apply proc xs))))
