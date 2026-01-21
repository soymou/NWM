#lang racket/gui

(provide setup-dark-text setup-dark-editor)

(define (setup-dark-text text)
  (define delta (new style-delta%))
  (send delta set-delta-foreground "white")
  (send delta set-delta-background (make-object color% 30 30 30))
  
  ;; 1. Update existing content
  (send text change-style delta)
  
  ;; 2. Update "Standard" style
  (define sl (send text get-style-list))
  (define standard (send sl find-named-style "Standard"))
  (when standard
    (send standard set-delta delta)))

(define (setup-dark-editor text canvas)
  (send canvas set-canvas-background (make-object color% 30 30 30))
  (setup-dark-text text))