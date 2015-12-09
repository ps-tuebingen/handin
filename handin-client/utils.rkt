#lang racket/base

(provide editors->string)

(require racket/class racket/gui/base)

; Separated for reusability. Should still stay on handin-client to be included in deploys of the plugin.
(define (editors->string definitions interactions)
  (let* ([base (make-object editor-stream-out-bytes-base%)]
         [stream (make-object editor-stream-out% base)])
    (write-editor-version stream base)
    (write-editor-global-header stream)
    (for ([ed (in-list (list definitions interactions))]) (send ed write-to-file stream))
    (write-editor-global-footer stream)
    (send base get-bytes)))
