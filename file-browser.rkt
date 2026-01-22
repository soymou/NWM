#lang racket/gui

(provide create-file-browser
         change-browser-directory)

(define current-dir (make-parameter (current-directory)))
(define browser-refresh-callback (make-parameter void))

;; Nerd Font Icons
(define icon-folder "\uf115")
(define icon-nix "\uf313")
(define icon-file "\uf15b")
(define icon-up "\uf062")
(define icon-refresh "\uf021")

(define (change-browser-directory path)
  (when (directory-exists? path)
    (current-dir path)
    ((browser-refresh-callback))))

(define (create-file-browser parent frame open-file-callback)
  (define panel (new vertical-panel% 
                     [parent parent] 
                     [min-width 250]
                     [stretchable-width #f]
                     [style '(border)]))
  
  ;; --- Header ---
  (define header (new horizontal-panel% 
                      [parent panel] 
                      [stretchable-height #f] 
                      [alignment '(left center)]
                      [spacing 2]
                      [border 5]))
  
  (new message% [parent header] [label "Explorer"] [font (make-object font% 10 'default 'normal 'bold)])
  
  (new panel% [parent header] [stretchable-width #t]) ;; Spacer

  (new button% 
       [parent header] 
       [label icon-up] 
       [min-width 30]
       [font (make-object font% 12 'modern)]
       [callback (lambda (b e)
                   (define parent-dir (simplify-path (build-path (current-dir) "..")))
                   (current-dir parent-dir)
                   ((browser-refresh-callback)))])

  (new button% 
       [parent header] 
       [label icon-refresh] 
       [min-width 30]
       [font (make-object font% 12 'modern)]
       [callback (lambda (b e) ((browser-refresh-callback)))])

  ;; --- Path Display ---
  (define path-field 
    (new text-field% 
         [parent panel] 
         [label #f]
         [init-value (path->string (current-dir))]
         [enabled #f]
         [style '(single)]))

  ;; --- File List ---
  (define file-list 
    (new list-box%
         [parent panel]
         [label #f]
         [choices '()]
         [style '(single)] ; No column headers for cleaner look
         [font (make-object font% 12 'modern)]
         [callback (lambda (lb e)
                     (when (eq? (send e get-event-type) 'list-box-dclick)
                       (define sel (send lb get-selection))
                       (when sel
                         (define name (send lb get-data sel))
                         (define full-path (build-path (current-dir) name))
                         
                         (cond
                           [(directory-exists? full-path)
                            (current-dir full-path)
                            ((browser-refresh-callback))]
                           [(file-exists? full-path)
                            (when (or (string-suffix? name ".nix")
                                      (string-suffix? name ".lock"))
                              (open-file-callback full-path))]))))]))

  (define (refresh-file-list)
    (send file-list clear)
    (send path-field set-value (path->string (current-dir)))
    
    (define content 
      (with-handlers ([exn:fail? (lambda (e) '())])
        (directory-list (current-dir))))
    
    (define dirs (filter (lambda (p) (directory-exists? (build-path (current-dir) p))) content))
    ;; Only show .nix files
    (define files (filter (lambda (p) 
                            (and (file-exists? (build-path (current-dir) p))
                                 (string-suffix? (path->string p) ".nix"))) 
                          content))
    
    ;; Add directories
    (for ([d (sort dirs path<?)])
      (define name (path->string d))
      (send file-list append (format "~a ~a" icon-folder name) name))
      
    ;; Add files
    (for ([f (sort files path<?)])
      (define name (path->string f))
      (send file-list append (format "~a ~a" icon-nix name) name)))

  ;; Register callback
  (browser-refresh-callback refresh-file-list)

  ;; Initial load
  (refresh-file-list)
  
  panel)
