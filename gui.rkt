#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")
(require "theme.rkt")
(require "parser.rkt")
(require "actions.rkt")
(require "search.rkt")

;; Flag to prevent recursive refresh loops
(define is-refreshing? (make-parameter #f))

;; =============================================================================
;; MAIN WINDOW & LAYOUT
;; =============================================================================

(define frame (new frame%
                   [label "Nix Workspace Manager"]
                   [width 1000]
                   [height 700]))

(main-frame frame)

;; --- MENU BAR ---
(define menu-bar (new menu-bar% [parent frame]))

(define m-file (new menu% [parent menu-bar] [label "File"]))
(new menu-item% [parent m-file] [label "New File (Set)"]
     [callback (lambda (i e) (handle-error (lambda () (reset-to-type! "set") (refresh-gui))))])
(new menu-item% [parent m-file] [label "New File (List)"]
     [callback (lambda (i e) (handle-error (lambda () (reset-to-type! "list") (refresh-gui))))])
(new menu-item% [parent m-file] [label "New File (Scope/Let)"]
     [callback (lambda (i e) (handle-error (lambda () (reset-to-type! "let") (refresh-gui))))])
(new separator-menu-item% [parent m-file])
(new menu-item% [parent m-file] [label "Save"]
     [callback (lambda (i e) (handle-error (lambda () (save-config! (current-buffer-name)) (message-box "Info" "Saved!" frame))))])
(new menu-item% [parent m-file] [label "Save As..."]
     [callback (lambda (i e)
                 (define f (put-file "Save as..." #f #f "default.nix"))
                 (when f
                   (handle-error (lambda () 
                                   (save-config! (path->string f))
                                   (switch-buffer! (path->string f))
                                   (refresh-gui)))))
                 ])
(new separator-menu-item% [parent m-file])
(new menu-item% [parent m-file] [label "Exit"]
     [callback (lambda (i e) (send frame show #f))])

(define m-edit (new menu% [parent menu-bar] [label "Edit"]))
(new menu-item% [parent m-edit] [label "Undo"]
     [shortcut #\z]
     [callback (lambda (i e) (handle-error (lambda () (undo!) (refresh-gui))))])

(define m-templates (new menu% [parent m-edit] [label "Templates"]))
(new menu-item% [parent m-templates] [label "Init Flake"]
     [callback (lambda (i e)
                 (handle-error (lambda () 
                                 (push-history!)
                                 (set-val! "inputs" (nix-set '()))
                                 (set-val! "outputs" (nix-set '()))
                                 (enter! "inputs")
                                 (set-val! "nixpkgs" (nix-set '()))
                                 (enter! "nixpkgs")
                                 (set-val! "url" "github:nixos/nixpkgs/nixos-unstable")
                                 (top!)
                                 (refresh-gui))))])

(new menu-item% [parent m-templates] [label "Init Shell"]
     [callback (lambda (i e)
                 (handle-error (lambda () 
                                 (push-history!)
                                 (set-val! "buildInputs" (nix-list '()))
                                 (set-val! "shellHook" "echo 'Welcome'")
                                 (refresh-gui))))])

;; --- TOOLBAR ---
(define toolbar-panel (new horizontal-panel% [parent frame] [stretchable-height #f] [min-height 40] [spacing 5] [border 5]))

(new button% [parent toolbar-panel] [label "Top"] [callback (lambda (b e) (handle-error (lambda () (top!) (refresh-gui))))])
(new button% [parent toolbar-panel] [label "Up"] [callback (lambda (b e) (handle-error (lambda () (up!) (refresh-gui))))])

(new panel% [parent toolbar-panel] [min-width 10] [stretchable-width #f])

(new button% [parent toolbar-panel] [label "Add..."]
     [callback (lambda (b e)
                 (define menu (new popup-menu%))
                 (new menu-item% [parent menu] [label "Set"] [callback (lambda (i e) (add-child-handler "set"))])
                 (new menu-item% [parent menu] [label "List"] [callback (lambda (i e) (add-child-handler "list"))])
                 (new separator-menu-item% [parent menu])
                 (new menu-item% [parent menu] [label "Value (Scalar)"] [callback (lambda (i e) (add-child-handler "value"))])
                 (send b popup-menu menu 0 0))])

(new button% [parent toolbar-panel] [label "Wrap Scope"] [callback (lambda (b e) (wrap-in-scope-handler))])
(new button% [parent toolbar-panel] [label "Wrap Lambda"] [callback (lambda (b e) (wrap-in-lambda-handler))])
(new button% [parent toolbar-panel] [label "Unwrap"] [callback (lambda (b e) (unwrap-handler))])
(new button% [parent toolbar-panel] [label "Update"] [callback (lambda (b e) (update-selected-handler))])
(new button% [parent toolbar-panel] [label "Comment"] [callback (lambda (b e) (comment-handler))])
(new button% [parent toolbar-panel] [label "Delete"] [callback (lambda (b e) (delete-selected-handler))])

(new panel% [parent toolbar-panel] [min-width 10] [stretchable-width #f])

(define path-msg (new message% [parent toolbar-panel] [label "/"] [auto-resize #t] [stretchable-width #t]))

(new panel% [parent toolbar-panel] [min-width 10] [stretchable-width #f])

(define search-toggle-btn
  (new button% [parent toolbar-panel] [label "Search"]
       [callback (lambda (b e) (toggle-search-panel))]))

;; =============================================================================
;; SEARCH PANEL STATE
;; =============================================================================

(define search-panel-visible? #t)
(define search-results '())  ;; List of package-result
(define search-thread #f)

;; =============================================================================
;; LAYOUT STRUCTURE
;; =============================================================================

;; Outer container holds main-split and search-panel
(define outer-container (new vertical-panel% [parent frame] [alignment '(center center)]))

;; --- MAIN SPLIT ---
(define main-split (new horizontal-panel% [parent outer-container] [alignment '(center center)]))

;; --- LEFT: TREE VIEW ---
(define left-panel (new vertical-panel% [parent main-split] [min-width 250] [stretchable-width #f] [spacing 0] [border 0]))
(new message% [parent left-panel] [label "Structure Tree"])

(define tree-view-text (new text%))
(define tree-view-canvas (new editor-canvas% 
                              [parent left-panel] 
                              [editor tree-view-text]
                              [style '(no-hscroll)]
                              [stretchable-width #t]
                              [stretchable-height #t]))
(setup-dark-editor tree-view-text tree-view-canvas)

;; --- RIGHT: EDITORS ---

;; Custom text% that syncs with tree on edits and cursor movement
(define sync-text%
  (class text%
    (super-new)
    (define/augment (on-change)
      (unless (is-refreshing?)
        (handle-auto-update)))
    (define/override (on-event event)
      (super on-event event)
      (when (send event button-down? 'left)
        (sync-path-from-pos (send this get-start-position))))
    (define/override (on-char event)
      (super on-char event)
      (define key (send event get-key-code))
      (when (member key '(left right up down))
        (sync-path-from-pos (send this get-start-position))))
    (define/override (set-position start [end 'same] [scroll #t] [select #f])
      (super set-position start end scroll select)
      (unless (is-refreshing?)
        (sync-path-from-pos start)))))

(define right-split (new vertical-panel% [parent main-split] [stretchable-width #t]))

(define top-right-group (new group-box-panel% [parent right-split] [label "Whole Source (Navigable)"] [stretchable-height #t]))
(define whole-source-text (new sync-text%))
(define whole-source-canvas (new editor-canvas% 
                                 [parent top-right-group] 
                                 [editor whole-source-text]
                                 [style '(no-hscroll)]))
(setup-dark-editor whole-source-text whole-source-canvas)
(whole-source-widget whole-source-text)

(define bottom-right-group (new group-box-panel% [parent right-split] [label "Current Node Source (Read Only)"] [stretchable-height #f] [min-height 200]))
(define source-view-text (new text%))
(define source-view-canvas (new editor-canvas%
                                [parent bottom-right-group]
                                [editor source-view-text]
                                [style '(no-hscroll)]))
(setup-dark-editor source-view-text source-view-canvas)
(send source-view-text lock #t)

;; =============================================================================
;; SEARCH PANEL
;; =============================================================================

(define search-panel
  (new group-box-panel%
       [parent outer-container]
       [label "Search"]
       [stretchable-height #f]
       [min-height 250]))

;; Search bar: type selector + text field + buttons
(define search-bar (new horizontal-panel% [parent search-panel] [stretchable-height #f] [alignment '(left center)]))

;; Search type selector
(define search-type-choice
  (new choice%
       [parent search-bar]
       [label "Type:"]
       [choices '("Packages" "Options")]
       [selection 0]
       [callback (lambda (c e)
                   ;; Update column headers based on search type
                   (update-search-columns))]))

(define search-field
  (new text-field%
       [parent search-bar]
       [label "Search:"]
       [min-width 300]
       [stretchable-width #t]
       [callback (lambda (tf e)
                   (when (eq? (send e get-event-type) 'text-field-enter)
                     (start-search (send tf get-value))))]))

(define search-btn
  (new button%
       [parent search-bar]
       [label "Search"]
       [callback (lambda (b e) (start-search (send search-field get-value)))]))

(define close-search-btn
  (new button%
       [parent search-bar]
       [label "Close"]
       [callback (lambda (b e) (hide-search-panel))]))

;; Status message
(define search-status
  (new message%
       [parent search-panel]
       [label "Enter a package name to search."]
       [auto-resize #t]
       [stretchable-width #t]))

;; Results list-box with columns: Package | Version | Description
(define results-list-box
  (new list-box%
       [parent search-panel]
       [label #f]
       [choices '()]
       [columns '("Package" "Version" "Description")]
       [column-order '(0 1 2)]
       [style '(single column-headers variable-columns)]
       [stretchable-height #t]
       [callback (lambda (lb e)
                   (when (eq? (send e get-event-type) 'list-box-dclick)
                     (define idx (send lb get-selection))
                     (when idx
                       (define item (list-ref search-results idx))
                       (if (= (get-search-type) 0)
                           (show-insert-dialog item)      ;; Package
                           (show-option-insert-dialog item)))))]))

;; Set column widths: Package (200), Version (100), Description (rest)
(send results-list-box set-column-width 0 200 50 500)
(send results-list-box set-column-width 1 100 50 150)
(send results-list-box set-column-width 2 400 100 1000)

;; Search panel is visible by default

;; =============================================================================
;; SEARCH FUNCTIONS
;; =============================================================================

(define (toggle-search-panel)
  (set! search-panel-visible? (not search-panel-visible?))
  (send search-panel show search-panel-visible?)
  (when search-panel-visible?
    (send search-field focus)))

(define (hide-search-panel)
  (set! search-panel-visible? #f)
  (send search-panel show #f))

(define (get-search-type)
  (send search-type-choice get-selection))  ;; 0 = Packages, 1 = Options

(define (update-search-columns)
  ;; Clear current results when switching types
  (set! search-results '())
  (send results-list-box clear)
  (send search-status set-label
        (if (= (get-search-type) 0)
            "Enter a package name to search."
            "Enter an option name to search (e.g., services.nginx).")))

(define (start-search query)
  (when (non-empty-string? (string-trim query))
    ;; Kill existing search thread if any
    (when (and search-thread (thread-running? search-thread))
      (kill-thread search-thread))

    ;; Clear results and show searching status
    (set! search-results '())
    (send results-list-box clear)
    (send search-status set-label "Searching...")

    (define search-type (get-search-type))

    ;; Start search in background thread
    (set! search-thread
          (thread
           (lambda ()
             (define-values (results error)
               (if (= search-type 0)
                   (search-packages query)
                   (search-options query)))
             (queue-callback
              (lambda ()
                (if (= search-type 0)
                    (display-package-results results error)
                    (display-option-results results error)))))))))

(define (display-package-results results error)
  (set! search-results results)
  (send results-list-box clear)

  (cond
    [error
     (send search-status set-label (format "Error: ~a" error))]
    [(null? results)
     (send search-status set-label "No packages found.")]
    [else
     (send search-status set-label (format "Found ~a packages." (length results)))
     (for ([pkg results])
       (send results-list-box append
             (format-package-name pkg)
             pkg)
       ;; Set column data
       (define row (sub1 (send results-list-box get-number)))
       (send results-list-box set-string row (package-result-version pkg) 1)
       (send results-list-box set-string row
             (let ([desc (package-result-description pkg)])
               (if (> (string-length desc) 80)
                   (string-append (substring desc 0 77) "...")
                   desc))
             2))]))

(define (display-option-results results error)
  (set! search-results results)
  (send results-list-box clear)

  (cond
    [error
     (send search-status set-label (format "Error: ~a" error))]
    [(null? results)
     (send search-status set-label "No options found.")]
    [else
     (send search-status set-label (format "Found ~a options." (length results)))
     (for ([opt results])
       (send results-list-box append
             (option-result-name opt)
             opt)
       ;; Set column data: Name | Type | Description
       (define row (sub1 (send results-list-box get-number)))
       (send results-list-box set-string row (option-result-type opt) 1)
       (send results-list-box set-string row
             (let ([desc (option-result-description opt)])
               (if (> (string-length desc) 80)
                   (string-append (substring desc 0 77) "...")
                   desc))
             2))]))

;; =============================================================================
;; INSERT DIALOG
;; =============================================================================

(define (show-insert-dialog pkg)
  (define dialog
    (new dialog%
         [parent frame]
         [label "Insert Package"]
         [width 400]
         [height 300]))

  ;; Package info
  (new message%
       [parent dialog]
       [label (format "Package: ~a" (format-package-name pkg))])
  (new message%
       [parent dialog]
       [label (format "Version: ~a" (package-result-version pkg))])
  (when (non-empty-string? (package-result-description pkg))
    (new message%
         [parent dialog]
         [label (format "~a" (substring (package-result-description pkg)
                                        0
                                        (min 100 (string-length (package-result-description pkg)))))]))

  (new message% [parent dialog] [label ""])
  (new message% [parent dialog] [label "Insert into:"])

  ;; Custom path field (defined first so radio-box callback can reference it)
  (define custom-path-field #f)

  ;; Target selection
  (define target-choice
    (new radio-box%
         [parent dialog]
         [label #f]
         [choices '("buildInputs (shell.nix)"
                    "environment.systemPackages (NixOS)"
                    "home.packages (home-manager)"
                    "packages (flake devShell)"
                    "Custom path...")]
         [selection 0]
         [callback (lambda (rb e)
                     (when custom-path-field
                       (send custom-path-field enable (= (send rb get-selection) 4))))]))

  ;; Custom path field
  (set! custom-path-field
    (new text-field%
         [parent dialog]
         [label "Custom path:"]
         [init-value ""]
         [enabled #f]))

  ;; Button panel
  (define btn-panel (new horizontal-panel% [parent dialog] [alignment '(right center)]))

  (define result #f)

  (new button%
       [parent btn-panel]
       [label "Cancel"]
       [callback (lambda (b e) (send dialog show #f))])

  (new button%
       [parent btn-panel]
       [label "Insert"]
       [style '(border)]
       [callback (lambda (b e)
                   (define sel (send target-choice get-selection))
                   (define target-path
                     (case sel
                       [(0) '("buildInputs")]
                       [(1) '("environment" "systemPackages")]
                       [(2) '("home" "packages")]
                       [(3) '("packages")]
                       [(4) (string-split (send custom-path-field get-value) ".")]))
                   (when (and (list? target-path) (not (null? target-path)))
                     (perform-package-insert pkg target-path)
                     (set! result #t)
                     (send dialog show #f)))])

  (send dialog show #t)
  result)

;; =============================================================================
;; INSERT LOGIC
;; =============================================================================

(define (perform-package-insert pkg target-path)
  (handle-error
   (lambda ()
     (push-history!)

     ;; Save current path
     (define orig-path (editor-state-path (current-session)))

     ;; Navigate to root
     (top!)

     ;; Navigate/create path to target
     (for ([seg target-path])
       (define s (current-session))
       (define root (editor-state-root s))
       (define path (editor-state-path s))
       (define node (get-node root path))

       ;; Check if child exists
       (define children (list-node-children node))
       (unless (member seg children)
         ;; Create the child as a list (since we're adding packages)
         (if (equal? seg (last target-path))
             (set-val! seg (nix-list '()))
             (set-val! seg (nix-set '()))))
       (enter! seg))

     ;; Now we're at the target - push the package
     (define pkg-name (format-package-name pkg))
     (push! (nix-var pkg-name))

     ;; Restore original path or stay at new location
     (top!)

     ;; Refresh GUI
     ((refresh-callback)))))

;; =============================================================================
;; OPTION INSERT DIALOG
;; =============================================================================

(define (show-option-insert-dialog opt)
  (define dialog
    (new dialog%
         [parent frame]
         [label "Insert Option"]
         [width 500]
         [height 350]))

  ;; Option info
  (new message%
       [parent dialog]
       [label (format "Option: ~a" (option-result-name opt))])
  (when (non-empty-string? (option-result-type opt))
    (new message%
         [parent dialog]
         [label (format "Type: ~a" (option-result-type opt))]))
  (when (non-empty-string? (option-result-description opt))
    (new message%
         [parent dialog]
         [label (format "~a"
                        (let ([desc (option-result-description opt)])
                          (if (> (string-length desc) 100)
                              (string-append (substring desc 0 97) "...")
                              desc)))]))

  (new message% [parent dialog] [label ""])

  ;; Value input
  (define value-field
    (new text-field%
         [parent dialog]
         [label "Value:"]
         [init-value (option-result-default opt)]
         [min-width 300]))

  (new message% [parent dialog] [label ""])
  (new message% [parent dialog] [label "The option path will be created automatically."])

  ;; Button panel
  (define btn-panel (new horizontal-panel% [parent dialog] [alignment '(right center)]))

  (define result #f)

  (new button%
       [parent btn-panel]
       [label "Cancel"]
       [callback (lambda (b e) (send dialog show #f))])

  (new button%
       [parent btn-panel]
       [label "Insert"]
       [style '(border)]
       [callback (lambda (b e)
                   (define value-str (send value-field get-value))
                   (when (non-empty-string? value-str)
                     (perform-option-insert opt value-str)
                     (set! result #t)
                     (send dialog show #f)))])

  (send dialog show #t)
  result)

(define (perform-option-insert opt value-str)
  (handle-error
   (lambda ()
     (push-history!)

     ;; Parse option path like "services.nginx.enable" -> '("services" "nginx" "enable")
     (define option-path (string-split (option-result-name opt) "."))

     ;; Navigate to root
     (top!)

     ;; Navigate/create path to the option's parent
     (define parent-path (drop-right option-path 1))
     (define option-name (last option-path))

     (for ([seg parent-path])
       (define s (current-session))
       (define root (editor-state-root s))
       (define path (editor-state-path s))
       (define node (get-node root path))

       ;; Check if child exists
       (define children (list-node-children node))
       (unless (member seg children)
         (set-val! seg (nix-set '())))
       (enter! seg))

     ;; Set the option value
     (define parsed-value (parse-value value-str))
     (set-val! option-name parsed-value)

     ;; Return to root
     (top!)

     ;; Refresh GUI
     ((refresh-callback)))))

;; =============================================================================
;; REFRESH LOGIC
;; =============================================================================

;; Tree State
(define expanded-paths (make-hash)) ;; path -> boolean
(hash-set! expanded-paths '() #t)   ;; Always expand root

(define (toggle-expand path)
  (let ([curr (hash-ref expanded-paths path #f)])
    (hash-set! expanded-paths path (not curr))
    (refresh-views-only)))

(define (select-path path)
  (let ([s (current-session)])
    (current-session (struct-copy editor-state s [path path])))
  (refresh-views-only))

(define (render-tree root current-path)
  (send tree-view-text erase)
  (send tree-view-text begin-edit-sequence)
  
  (define (recurse node path level)
    (define is-container? (or (nix-set? node) (nix-list? node) (nix-let? node) (nix-lambda? node)))
    (define is-expanded? (hash-ref expanded-paths path #f))
    (define is-selected? (equal? path current-path))
    (define base-label (if (empty? path) "/" (last path)))
    (define label 
      (cond
        [(nix-let? node) (string-append base-label " [let]")]
        [(nix-lambda? node) (format "~a ({ ~a }:) " base-label (string-join (nix-lambda-args node) ", "))]
        [else base-label]))
    
    (if is-container?
        (let* ([toggle-str (if is-expanded? "[-] " "[+] ")]
               [start (send tree-view-text last-position)])
          (send tree-view-text insert (make-string (* level 2) #\space))
          
          ;; Toggle
          (let* ([toggle-start (send tree-view-text last-position)])
            (send tree-view-text insert toggle-str)
            (let ([toggle-end (send tree-view-text last-position)])
              (send tree-view-text set-clickback toggle-start toggle-end (lambda (t s e) (toggle-expand path)))
              
              ;; Label
              (let* ([label-start (send tree-view-text last-position)])
                (send tree-view-text insert label)
                (let ([label-end (send tree-view-text last-position)])
                  (send tree-view-text set-clickback label-start label-end (lambda (t s e) (select-path path)))
                  
                  (send tree-view-text insert "\n")
                  
                  ;; Highlight Label
                  (when is-selected?
                    (define delta (new style-delta%))
                    (send delta set-delta-background (make-object color% 60 60 90))
                    (send delta set-delta-foreground "white")
                    (send tree-view-text change-style delta label-start label-end))
                  
                  ;; Recurse children if expanded
                  (when is-expanded?
                    (match node
                      [(struct nix-set (bindings))
                       (for ([b bindings])
                         (recurse (binding-value b) (append path (list (binding-name b))) (add1 level)))]
                      [(struct nix-list (elems))
                       (for ([e elems] [i (in-naturals)])
                         (recurse e (append path (list (number->string i))) (add1 level)))]
                      [(struct nix-let (bindings body))
                       (begin
                         (recurse (nix-set bindings) (append path (list "bindings")) (add1 level))
                         (recurse body (append path (list "body")) (add1 level)))]
                      [(struct nix-lambda (args body))
                       (recurse body (append path (list "body")) (add1 level))]
                      [_ (void)])))))))
        
        ;; Leaf
        (let* ([start (send tree-view-text last-position)])
          (send tree-view-text insert (make-string (* level 2) #\space))
          (let* ([label-start (send tree-view-text last-position)])
            (send tree-view-text insert label)
            (let ([label-end (send tree-view-text last-position)])
              (send tree-view-text insert "\n")
              
              (send tree-view-text set-clickback label-start label-end (lambda (t s e) (select-path path)))
              
              (when is-selected?
                (define delta (new style-delta%))
                (send delta set-delta-background (make-object color% 60 60 90))
                (send delta set-delta-foreground "white")
                (send tree-view-text change-style delta label-start label-end)))))))

  (recurse root '() 0)
  (send tree-view-text end-edit-sequence))

(define (refresh-views-only [update-node-view? #t])
  (define s (current-session))
  (define root (editor-state-root s))
  (define path (editor-state-path s))
  
  (when update-node-view?
    (define node (get-node root path))
    (send source-view-text lock #f)
    (send source-view-text erase)
    (send source-view-text insert (to-nix node))
    (send source-view-text lock #t))
  
  (send path-msg set-label (format "Path: ~a" (format-path path)))
  (render-tree root path))

(define (refresh-gui [full? #t])
  (is-refreshing? #t)
  
  (define s (current-session))
  (define root (editor-state-root s))
  
  (when full?
    ;; Regenerate Whole Source & Map
    (define-values (code mapping) (to-nix-mapped root))
    (current-sourcemap mapping)
    
    (send whole-source-text erase)
    (send whole-source-text insert code))
  
  (refresh-views-only)
  (is-refreshing? #f))

;; Register the refresh callback in actions.rkt
(refresh-callback refresh-gui)

;; Initialize
(refresh-gui)
(send frame show #t)