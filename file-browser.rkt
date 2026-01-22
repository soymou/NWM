#lang racket/gui

(provide create-file-browser)

(define current-dir (make-parameter (current-directory)))

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
                      [spacing 2]))
  
  (new message% [parent header] [label "Explorer"])
  
  (new panel% [parent header] [stretchable-width #t]) ;; Spacer
  
  (new button% 
       [parent header] 
       [label "Open"] 
       [callback (lambda (b e)
                   (define dir (get-directory "Select Directory" frame))
                   (when dir
                     (current-dir dir)
                     (refresh-file-list)))])

  (new button% 
       [parent header] 
       [label "↑"] 
       [min-width 30]
       [callback (lambda (b e)
                   (define parent-dir (simplify-path (build-path (current-dir) "..")))
                   (current-dir parent-dir)
                   (refresh-file-list))])

  (new button% 
       [parent header] 
       [label "↻"] 
       [min-width 30]
       [callback (lambda (b e) (refresh-file-list))])

  ;; --- Path Display ---
  (define path-field 
    (new text-field% 
         [parent panel] 
         [label #f]
         [init-value (path->string (current-dir))]
         [enabled #f]))

  ;; --- File List ---
  (define file-list 
    (new list-box%
         [parent panel]
         [label #f]
         [choices '()]
         [style '(single column-headers)]
         [columns '("Name")]
         [callback (lambda (lb e)
                     (when (eq? (send e get-event-type) 'list-box-dclick)
                       (define sel (send lb get-selection))
                       (when sel
                         (define item-text (send lb get-string sel))
                         ;; Remove trailing slash for directories if present (visual only)
                         (define name (if (string-suffix? item-text "/")
                                          (substring item-text 0 (sub1 (string-length item-text)))
                                          item-text))
                                          
                         (define full-path (build-path (current-dir) name))
                         
                         (cond
                           [(directory-exists? full-path)
                            (current-dir full-path)
                            (refresh-file-list)]
                           [(file-exists? full-path)
                            (when (or (string-suffix? name ".nix")
                                      (string-suffix? name ".lock")) ; Allow opening lock files as text?
                              (open-file-callback full-path))]))))]))

  (define (refresh-file-list)
    (send file-list clear)
    (send path-field set-value (path->string (current-dir)))
    
    (define content 
      (with-handlers ([exn:fail? (lambda (e) '())])
        (directory-list (current-dir))))
    
    (define dirs (filter (lambda (p) (directory-exists? (build-path (current-dir) p))) content))
    (define files (filter (lambda (p) (file-exists? (build-path (current-dir) p))) content))
    
    ;; Add directories with trailing slash
    (for ([d (sort dirs path<?)])
      (send file-list append (string-append (path->string d) "/")))
      
    ;; Add files
    (for ([f (sort files path<?)])
      (send file-list append (path->string f))))

  ;; Initial load
  (refresh-file-list)
  
  panel)
