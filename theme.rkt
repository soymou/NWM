#lang racket/gui

(provide setup-dark-text setup-dark-editor setup-dark-list-box
         dark-bg-color dark-fg-color dark-selection-color)

;; Theme colors
(define dark-bg-color (make-object color% 30 30 30))
(define dark-fg-color (make-object color% 255 255 255))
(define dark-selection-color (make-object color% 60 60 90))

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
  (send canvas set-canvas-background dark-bg-color)
  (setup-dark-text text))

;; Setup dark theme for list-box%
;; Note: Racket's list-box% has limited styling options, but we can set colors for selection
(define (setup-dark-list-box lb)
  ;; list-box% doesn't support extensive theming in Racket, but we ensure consistency
  (void))