#lang racket

; expects a CSV file with the users in the format
; id,name,username,email,title,created_at,last_seen_at,last_posted_at,last_emailed_at,trust_level,approved,suspended_at,suspended_till,blocked,active,admin,moderator,ip_address,topics_entered,posts_read_count,time_read,topic_count,post_count,likes_given,likes_received,tuesday_16 (custom user field),Programmiererfahrung (custom user field),wednesday_14 (custom user field),Studienabschluss (custom user field),wednesday_16 (custom user field),thursday_08 (custom user field),Programmiersprachen (custom user field),thursday_10 (custom user field),thursday_12 (custom user field),Anderer Studiengang (custom user field),thursday_16 (custom user field),Studiengang (custom user field),tuesday_08 (custom user field),Vorbildung (custom user field),assigned_group (custom user field),Tutoriumstermin (custom user field),tuesday_10 (custom user field),Fachsemester Informatik (custom user field),Matrikelnummer (custom user field),tuesday_12 (custom user field),group_names

(require (planet neil/csv:1:=7))

(provide all user->studiengang)

(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars               #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row
      (make-food-csv-reader (open-input-file "/Users/klaus/git/handin/users.csv")))

(define ignore (next-row)) ; skip header

(define (all-users)
  (let ((x (next-row)))
    (if (empty? x)
        empty
        (cons x (all-users)))))

(define all (all-users))

(define user->studiengang
  (make-hash
   (map (lambda (l) (cons (string-downcase (list-ref l 2)) (list-ref l 36))) all)))
   ;(map (lambda (l) (cons (list-ref l 2) (list-ref l 36))) all)))

; (list-of X) (X -> String) -> (hash String Number)
;(define (count-by-studiengang data getuser)
  ;(make-hash (map (lambda (x) (cons (f (first x)) x)) (group-by f l)))

;(map (lambda (g) (cons (length g) (first g))) (group-by (lambda (x) x)  (hash-values user->studiengang)))

;(define a (map (lambda (x) (string-append "@" (car x))) (filter (lambda (x) (string=? (cdr x) "")) (hash->list user->studiengang))))

;(define tutors (list "@lukas_hehle" "@Dragondeath" "@sophia_mersmann" "@bassal_rojeh" "@ajung" "@benjamin_steinert" "@Corni" "@Lou" "@alex" "@fabian_bauer" "@jannis_ploeger" "@dominik_heinrich" "@robin" "@simon_wegendt" "@MarcWeitz" "@fabian_mikulasch" "@Finn" "@jakob_koschel" "@felix_holzwarth" "@ture_sayer" "@Yahiko" "@Nevermind" "@felix_leitz" "@danielM" "@wiebke" "@Jannik" "@tim" "@franz_poeschel" "@Maxine"))

;  (for ([q a] #:when (not (member q tutors)))
;     (display q)
;     (display " "))