#lang racket/base
(provide timeout-control get-user-assignment-directory get-assignment-name get-client-plugin-version) ; General utilities, reexported by ../utils.rkt.

; Only exposed for main
(provide current-timeout-control user-assignment-directory assignment-name client-plugin-version)

(require "logger.rkt")
(define current-timeout-control (make-parameter #f))
(define (timeout-control msg)
  (log-line "timeout-control: ~s" msg)
  ((current-timeout-control) msg))

(define user-assignment-directory (make-parameter #f))
(define (get-user-assignment-directory) (user-assignment-directory))

(define assignment-name (make-parameter #f))
(define (get-assignment-name) (assignment-name))

(define client-plugin-version (make-parameter #f))
(define (get-client-plugin-version) (client-plugin-version))
