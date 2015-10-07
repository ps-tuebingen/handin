#lang racket/base

(require racket/file
         json
         net/http-client
         net/uri-codec
         "logger.rkt"
         "config.rkt")

;; Access user data for a user.
(provide get-user-data)
(define (get-user-data user)
  (or (get-user-data/local user)
      (get-user-data/discourse user)))

(provide canonicalize-username-case)
(define (canonicalize-username-case username)
  (if (get-conf 'username-case-sensitive)
      username
      (string-foldcase username)))

(define get-user-data/local
  (let ([users-file (build-path server-dir "users.rktd")])
    (unless (file-exists? users-file)
      (log-line "WARNING: users file missing on startup: ~a" users-file))
    (lambda (user)
      (and user (get-preference (string->symbol user) (lambda () #f) 'timestamp
                                users-file)))))

;; cache results of action for up to cache-timeout-ms milliseconds
(define (cached cache-timeout-ms action)
  (let ([cache #f]
        [last #f])
    (lambda ()
      (if (and last (< (- (current-inexact-milliseconds) last) cache-timeout-ms))
        cache
        (let ([result (action)])
          (set! cache result)
          (set! last (current-inexact-milliseconds))
          result)))))

;; access discourse configuration
(define get-conf/discourse
  (let* ([read-discourse-config-file
          (lambda ()
            (let* ([file (get-conf 'discourse-config-file)]
                   [text (and file (file->string file))]
                   [result (and text (string->jsexpr text))])
              result))]
         [discourse-config
          (cached 2000.0 read-discourse-config-file)])
    (lambda (key)
      (let ([config (discourse-config)])
        (and config (hash-ref config key))))))

(define DEBUG #f)
;; send request to discourse
(define (discourse-req path #:post-data [post-data #f] #:get-params [get-params '()])
  (let* ([api-username (get-conf/discourse 'api_username)]
         [api-key (get-conf/discourse 'api_key)]
         [api-endpoint-hostname (get-conf/discourse 'api_endpoint_hostname)]
         [method (if post-data "POST" "GET")]
         [full-path (format "~a?~a"
                            path
                            (alist->form-urlencoded `((api_key . ,api-key)
                                                      (api_username . ,api-username)
                                                      ,@get-params)))])
    (and api-username api-key
      (let-values ([(status header port)
                    (http-sendrecv api-endpoint-hostname
                                   full-path
                                   #:ssl? #t
                                   #:version "1.1"
                                   #:method method
                                   #:data post-data)])
        (if DEBUG
            ; Careful: This logs sensitive data.
            (log-line  "DISCOURSE ~a ~a: ~a ~a" method full-path post-data status)
            (log-line  "DISCOURSE ~a ~a: ~a ~a" method path status))
        (define result (read-json port))
        (close-input-port port)
        result))))

(define (to-ruby-boolean bool)
  (if bool "true" "false"))

;; fetch user database of discourse
(define get-user-data/discourse
  (let* ([fetch-data
          (lambda ()
            (let* ([allow-staff (to-ruby-boolean (get-conf 'discourse-auth-staff))]
                   [response (discourse-req "/admin/course/dump.json" #:get-params `((staff . ,allow-staff)))]
                   [users (and response
                               (hash-ref response 'success #f)
                               (hash-ref response 'users))])
              (if users
                  (for/hash ([user users])
                    (values (canonicalize-username-case (hash-ref user 'username))
                            (cons (list 'discourse (hash-ref user 'username))
                                  (for/list ([extra-field (get-conf 'extra-fields)])
                                    (hash-ref user (string->symbol (car extra-field)))))))
                  (hash))))]
         [data (cached 2000.0 fetch-data)])
    (lambda (username)
      (hash-ref (data) username #f))))

;; authenticate username/password with discourse
;; Discourse itself is case-insensitive.
;; Because usernames are also looked up in the DB produced by
;; get-user-data/discourse, and because get-user-data/discourse uses canonicalize-username-case,
;; we end up implementing correctly both settings of 'username-case-sensitive in config.rktd.
(define (has-password/discourse? username password)
  (hash-ref (discourse-req "/admin/course/auth.json"
                           #:post-data (alist->form-urlencoded `((user . ,username)
                                                                 (password . ,password))))
            'success))

(define crypt
  (let ([c #f] [sema (make-semaphore 1)])
    ;; use only when needed so it doesn't blow up on non-unix platforms
    (lambda (passwd salt)
      (unless c (set! c (dynamic-require 'ffi/crypt 'crypt)))
      ;; crypt is not reentrant
      (call-with-semaphore sema
                           (lambda () (bytes->string/utf-8 (c passwd salt)))))))

(provide make-has-password?)
(define ((make-has-password? error*) raw md5 passwords)
  (define (good? passwd)
    (define (bad-password msg)
      (log-line "ERROR: ~a -- ~s" msg passwd)
      (error* "bad password in user database"))
    (cond [(string? passwd) (equal? md5 passwd)]
          [(and (list? passwd) (= 2 (length passwd))
                (symbol? (car passwd)) (string? (cadr passwd)))
           (case (car passwd)
             [(plaintext) (equal? raw (cadr passwd))]
             [(unix)
              (let ([salt (regexp-match #rx"^([$][^$]+[$][^$]+[$]|..)"
                                        (cadr passwd))])
                (unless salt (bad-password "badly formatted unix password"))
                (equal? (crypt raw (car salt)) (cadr passwd)))]
             [(discourse)
              (has-password/discourse? (cadr passwd) raw)]
             [else (bad-password "bad password type in user database")])]
          [else (bad-password "bad password value in user database")]))
  (or (member md5 passwords) ; very cheap search first
      (ormap good? passwords)))
