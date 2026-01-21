#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")
(require "theme.rkt")
(require "parser.rkt")
(require "actions.rkt")
(require "search.rkt")
(require framework)

;; Flag to prevent recursive refresh loops
(define is-refreshing? (make-parameter #f))

;; =============================================================================
;; MAIN WINDOW & LAYOUT
;; =============================================================================

;; Custom frame with keyboard shortcuts
(define nwm-frame%
  (class frame%
    (super-new)
    (define/override (on-subwindow-char receiver event)
      (define key (send event get-key-code))
      (cond
        ;; Escape closes search panel
        [(eq? key 'escape)
         (when search-panel-visible?
           (hide-search-panel))
         #t]
        [else (super on-subwindow-char receiver event)]))))

(define frame (new nwm-frame%
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
     [shortcut #\s]
     [callback (lambda (i e) (handle-error (lambda () (save-config! (current-buffer-name)) (mark-saved!) (message-box "Info" "Saved!" frame))))])
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

(define m-view (new menu% [parent menu-bar] [label "View"]))
(new menu-item% [parent m-view] [label "Toggle Search Panel"]
     [shortcut #\f]
     [callback (lambda (i e) (toggle-search-panel))])
(new menu-item% [parent m-view] [label "Focus Search"]
     [shortcut #\g]
     [callback (lambda (i e)
                 (when (not search-panel-visible?)
                   (toggle-search-panel))
                 (send search-field focus))])

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

(new separator-menu-item% [parent m-templates])

(new menu-item% [parent m-templates] [label "NixOS Configuration"]
     [callback (lambda (i e)
                 (handle-error (lambda ()
                                 (push-history!)
                                 (reset-to-type! "set")
                                 (set-val! "imports" (nix-list '()))
                                 (set-val! "boot" (nix-set '()))
                                 (enter! "boot")
                                 (set-val! "loader" (nix-set '()))
                                 (enter! "loader")
                                 (set-val! "systemd-boot" (nix-set '()))
                                 (enter! "systemd-boot")
                                 (set-val! "enable" #t)
                                 (top!)
                                 (set-val! "networking" (nix-set '()))
                                 (enter! "networking")
                                 (set-val! "hostName" "nixos")
                                 (top!)
                                 (set-val! "time" (nix-set '()))
                                 (enter! "time")
                                 (set-val! "timeZone" "UTC")
                                 (top!)
                                 (set-val! "users" (nix-set '()))
                                 (enter! "users")
                                 (set-val! "users" (nix-set '()))
                                 (top!)
                                 (set-val! "environment" (nix-set '()))
                                 (enter! "environment")
                                 (set-val! "systemPackages" (nix-list '()))
                                 (top!)
                                 (set-val! "system" (nix-set '()))
                                 (enter! "system")
                                 (set-val! "stateVersion" "24.11")
                                 (top!)
                                 (refresh-gui))))])

(new menu-item% [parent m-templates] [label "Home Manager Configuration"]
     [callback (lambda (i e)
                 (handle-error (lambda ()
                                 (push-history!)
                                 (reset-to-type! "set")
                                 (set-val! "home" (nix-set '()))
                                 (enter! "home")
                                 (set-val! "username" "user")
                                 (set-val! "homeDirectory" "/home/user")
                                 (set-val! "stateVersion" "24.11")
                                 (set-val! "packages" (nix-list '()))
                                 (top!)
                                 (set-val! "programs" (nix-set '()))
                                 (set-val! "services" (nix-set '()))
                                 (refresh-gui))))])

(new menu-item% [parent m-templates] [label "Flake with DevShell"]
     [callback (lambda (i e)
                 (handle-error (lambda ()
                                 (push-history!)
                                 (reset-to-type! "set")
                                 (set-val! "description" "Development environment")
                                 (set-val! "inputs" (nix-set '()))
                                 (enter! "inputs")
                                 (set-val! "nixpkgs" (nix-set '()))
                                 (enter! "nixpkgs")
                                 (set-val! "url" "github:nixos/nixpkgs/nixos-unstable")
                                 (top!)
                                 (enter! "inputs")
                                 (set-val! "flake-utils" (nix-set '()))
                                 (enter! "flake-utils")
                                 (set-val! "url" "github:numtide/flake-utils")
                                 (top!)
                                 (set-val! "outputs" (nix-set '()))
                                 (refresh-gui))))])

(new menu-item% [parent m-templates] [label "Flake with NixOS Config"]
     [callback (lambda (i e)
                 (handle-error (lambda ()
                                 (push-history!)
                                 (reset-to-type! "set")
                                 (set-val! "description" "NixOS configuration")
                                 (set-val! "inputs" (nix-set '()))
                                 (enter! "inputs")
                                 (set-val! "nixpkgs" (nix-set '()))
                                 (enter! "nixpkgs")
                                 (set-val! "url" "github:nixos/nixpkgs/nixos-unstable")
                                 (top!)
                                 (set-val! "outputs" (nix-set '()))
                                 (enter! "outputs")
                                 (set-val! "nixosConfigurations" (nix-set '()))
                                 (top!)
                                 (refresh-gui))))])

(new menu-item% [parent m-templates] [label "Overlay"]
     [callback (lambda (i e)
                 (handle-error (lambda ()
                                 (push-history!)
                                 (reset-to-type! "set")
                                 ;; Overlay is typically: final: prev: { ... }
                                 ;; We'll create a simple set structure
                                 (refresh-gui))))])

;; --- TOOLBAR ---
(define toolbar-panel (new horizontal-panel% [parent frame] [stretchable-height #f] [min-height 40] [spacing 5] [border 5]))

;; Tooltip system
(define tooltip-frame #f)
(define tooltip-timer #f)

(define (show-tooltip text x y)
  (when tooltip-frame (send tooltip-frame show #f))
  (set! tooltip-frame (new frame% [label ""] [style '(no-caption float)] [width 10] [height 10]))
  (define msg (new message% [parent tooltip-frame] [label text]))
  (send tooltip-frame reflow-container)
  (send tooltip-frame move x y)
  (send tooltip-frame show #t))

(define (hide-tooltip)
  (when tooltip-frame
    (send tooltip-frame show #f)
    (set! tooltip-frame #f))
  (when tooltip-timer
    (send tooltip-timer stop)))

(define (make-tooltip-button parent label tooltip callback)
  (define btn
    (new button%
         [parent parent]
         [label label]
         [callback callback]))
  (define (on-enter)
    (when tooltip-timer (send tooltip-timer stop))
    (set! tooltip-timer
          (new timer%
               [notify-callback
                (lambda ()
                  (define-values (x y) (send btn client->screen 0 (send btn get-height)))
                  (show-tooltip tooltip x (+ y 5)))]
               [interval 500]
               [just-once? #t])))
  (define (on-leave)
    (hide-tooltip))
  ;; Note: button% doesn't have enter/leave events, so tooltips won't work perfectly
  ;; For now, just create the button
  btn)

(make-tooltip-button toolbar-panel "Top" "Go to root node" (lambda (b e) (handle-error (lambda () (top!) (refresh-gui)))))
(make-tooltip-button toolbar-panel "Up" "Go to parent node" (lambda (b e) (handle-error (lambda () (up!) (refresh-gui)))))

(new panel% [parent toolbar-panel] [min-width 10] [stretchable-width #f])

(make-tooltip-button toolbar-panel "Add..." "Add child node"
     (lambda (b e)
       (define menu (new popup-menu%))
       (new menu-item% [parent menu] [label "Set"] [callback (lambda (i e) (add-child-handler "set"))])
       (new menu-item% [parent menu] [label "List"] [callback (lambda (i e) (add-child-handler "list"))])
       (new separator-menu-item% [parent menu])
       (new menu-item% [parent menu] [label "Value (Scalar)"] [callback (lambda (i e) (add-child-handler "value"))])
       (send b popup-menu menu 0 0)))

(make-tooltip-button toolbar-panel "Wrap Scope" "Wrap in let...in block" (lambda (b e) (wrap-in-scope-handler)))
(make-tooltip-button toolbar-panel "Wrap Lambda" "Wrap in lambda function" (lambda (b e) (wrap-in-lambda-handler)))
(make-tooltip-button toolbar-panel "Unwrap" "Remove let/lambda wrapper" (lambda (b e) (unwrap-handler)))
(make-tooltip-button toolbar-panel "Update" "Rename key or update value" (lambda (b e) (update-selected-handler)))
(make-tooltip-button toolbar-panel "Comment" "Add comment to node" (lambda (b e) (comment-handler)))
(make-tooltip-button toolbar-panel "Delete" "Delete selected node" (lambda (b e) (delete-selected-handler)))

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
(define loading-timer #f)
(define loading-dots 0)

(define (start-loading-animation)
  (set! loading-dots 0)
  (when loading-timer (send loading-timer stop))
  (set! loading-timer
        (new timer%
             [notify-callback
              (lambda ()
                (set! loading-dots (modulo (add1 loading-dots) 4))
                (send search-status set-label
                      (string-append "Searching" (make-string loading-dots #\.))))]
             [interval 300])))

(define (stop-loading-animation)
  (when loading-timer
    (send loading-timer stop)
    (set! loading-timer #f)))

;; =============================================================================
;; LAYOUT STRUCTURE
;; =============================================================================

;; Outer container holds main-split and search-panel (dragable for resizing)
(define outer-container (new panel:vertical-dragable% [parent frame]))

;; --- MAIN SPLIT ---
(define main-split (new panel:horizontal-dragable% [parent outer-container]))

;; --- LEFT: TREE VIEW ---
(define left-panel (new vertical-panel% [parent main-split] [min-width 200] [stretchable-width #t] [spacing 0] [border 0]))

;; Tree header with expand/collapse buttons
(define tree-header (new horizontal-panel% [parent left-panel] [stretchable-height #f] [alignment '(left center)]))
(new message% [parent tree-header] [label "Structure Tree"])
(new panel% [parent tree-header] [stretchable-width #t])  ;; Spacer
(new button% [parent tree-header] [label "▼"] [min-width 30]
     [callback (lambda (b e) (expand-all!))])
(new button% [parent tree-header] [label "▲"] [min-width 30]
     [callback (lambda (b e) (collapse-all!))])

(define tree-view-text (new text%))

;; Custom canvas with right-click context menu
(define tree-context-canvas%
  (class editor-canvas%
    (super-new)
    (define/override (on-event event)
      (when (send event button-down? 'right)
        (show-tree-context-menu (send event get-x) (send event get-y)))
      (super on-event event))))

(define tree-view-canvas (new tree-context-canvas%
                              [parent left-panel]
                              [editor tree-view-text]
                              [style '(no-hscroll)]
                              [stretchable-width #t]
                              [stretchable-height #t]))
(setup-dark-editor tree-view-text tree-view-canvas)

;; Tree context menu
(define (show-tree-context-menu x y)
  (define menu (new popup-menu%))
  (new menu-item% [parent menu] [label "Add Set"]
       [callback (lambda (i e) (add-child-handler "set"))])
  (new menu-item% [parent menu] [label "Add List"]
       [callback (lambda (i e) (add-child-handler "list"))])
  (new menu-item% [parent menu] [label "Add Value"]
       [callback (lambda (i e) (add-child-handler "value"))])
  (new separator-menu-item% [parent menu])
  (new menu-item% [parent menu] [label "Update"]
       [callback (lambda (i e) (update-selected-handler))])
  (new menu-item% [parent menu] [label "Delete"]
       [callback (lambda (i e) (delete-selected-handler))])
  (new separator-menu-item% [parent menu])
  (new menu-item% [parent menu] [label "Wrap in Scope"]
       [callback (lambda (i e) (wrap-in-scope-handler))])
  (new menu-item% [parent menu] [label "Wrap in Lambda"]
       [callback (lambda (i e) (wrap-in-lambda-handler))])
  (new menu-item% [parent menu] [label "Unwrap"]
       [callback (lambda (i e) (unwrap-handler))])
  (new separator-menu-item% [parent menu])
  (new menu-item% [parent menu] [label "Add Comment"]
       [callback (lambda (i e) (comment-handler))])
  (send tree-view-canvas popup-menu menu x y))

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
       [choices '("Packages" "NixOS Options" "Home Manager Options")]
       [selection 0]
       [callback (lambda (c e)
                   ;; Update column headers based on search type
                   (update-search-columns))]))

;; Search history
(define search-history '())
(define max-history-size 20)

(define (add-to-search-history query)
  (when (non-empty-string? (string-trim query))
    ;; Remove duplicates and add to front
    (set! search-history
          (take (cons query (remove query search-history))
                (min max-history-size (add1 (length search-history)))))))

(define search-field
  (new text-field%
       [parent search-bar]
       [label "Search:"]
       [min-width 300]
       [stretchable-width #t]
       [callback (lambda (tf e)
                   (when (eq? (send e get-event-type) 'text-field-enter)
                     (start-search (send tf get-value))))]))

(define history-btn
  (new button%
       [parent search-bar]
       [label "▼"]
       [min-width 30]
       [callback (lambda (b e)
                   (define menu (new popup-menu%))
                   (if (null? search-history)
                       (new menu-item% [parent menu] [label "(No history)"]
                            [callback (lambda (i e) (void))])
                       (for ([query search-history])
                         (new menu-item% [parent menu] [label query]
                              [callback (lambda (i e)
                                          (send search-field set-value query)
                                          (start-search query))])))
                   ;; Simple popup on the search panel
                   (send search-panel popup-menu menu
                         (send b get-x)
                         (+ (send b get-y) (send b get-height))))]))

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
       [columns '("Name" "Type/Version" "Description")]
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
;; STATUS BAR
;; =============================================================================

(define status-bar (new horizontal-panel% [parent frame] [stretchable-height #f] [alignment '(left center)]))
(define status-file-label (new message% [parent status-bar] [label "File: default.nix"] [auto-resize #t]))
(new panel% [parent status-bar] [min-width 20] [stretchable-width #f])
(define status-modified-label (new message% [parent status-bar] [label ""] [auto-resize #t]))
(new panel% [parent status-bar] [stretchable-width #t])  ;; Spacer
(define status-path-label (new message% [parent status-bar] [label "Path: /"] [auto-resize #t]))

;; Track modification state
(define is-modified? #f)

(define (update-status-bar)
  (send status-file-label set-label (format "File: ~a" (current-buffer-name)))
  (send status-modified-label set-label (if is-modified? "[Modified]" ""))
  (send status-path-label set-label (format "Path: ~a" (format-path (editor-state-path (current-session))))))

(define (mark-modified!)
  (set! is-modified? #t)
  (update-status-bar))

(define (mark-saved!)
  (set! is-modified? #f)
  (update-status-bar))

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
  (send search-type-choice get-selection))  ;; 0 = Packages, 1 = NixOS Options, 2 = Home Manager Options

(define (update-search-columns)
  ;; Clear current results when switching types
  (set! search-results '())
  (send results-list-box clear)
  (define search-type (get-search-type))
  (send search-status set-label
        (case search-type
          [(0) "Enter a package name to search."]
          [(1) "Enter a NixOS option name (e.g., services.nginx)."]
          [(2) "Enter a Home Manager option (e.g., programs.git)."])))

(define (start-search query)
  (when (non-empty-string? (string-trim query))
    ;; Add to search history
    (add-to-search-history query)

    ;; Kill existing search thread if any
    (when (and search-thread (thread-running? search-thread))
      (kill-thread search-thread))

    ;; Clear results and show searching status
    (set! search-results '())
    (send results-list-box clear)
    (start-loading-animation)

    (define search-type (get-search-type))

    ;; Start search in background thread
    (set! search-thread
          (thread
           (lambda ()
             (define-values (results error)
               (case search-type
                 [(0) (search-packages query)]
                 [(1) (search-options query "nixos_options")]
                 [(2) (search-options query "hm_options")]))
             (queue-callback
              (lambda ()
                (if (= search-type 0)
                    (display-package-results results error)
                    (display-option-results results error)))))))))

(define (display-package-results results error)
  (stop-loading-animation)
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
  (stop-loading-animation)
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

(define (expand-all!)
  ;; Recursively expand all container nodes
  (define (expand-node node path)
    (when (or (nix-set? node) (nix-list? node) (nix-let? node) (nix-lambda? node))
      (hash-set! expanded-paths path #t)
      (match node
        [(struct nix-set (bindings))
         (for ([b bindings])
           (expand-node (binding-value b) (append path (list (binding-name b)))))]
        [(struct nix-list (elems))
         (for ([e elems] [i (in-naturals)])
           (expand-node e (append path (list (number->string i)))))]
        [(struct nix-let (bindings body))
         (expand-node (nix-set bindings) (append path (list "bindings")))
         (expand-node body (append path (list "body")))]
        [(struct nix-lambda (args body))
         (expand-node body (append path (list "body")))]
        [_ (void)])))
  (expand-node (editor-state-root (current-session)) '())
  (refresh-views-only))

(define (collapse-all!)
  ;; Collapse all nodes except root
  (hash-clear! expanded-paths)
  (hash-set! expanded-paths '() #t)
  (refresh-views-only))

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
    ;; Type icons for better visual distinction
    (define type-icon
      (cond
        [(nix-set? node) "{ } "]
        [(nix-list? node) "[ ] "]
        [(nix-let? node) "let "]
        [(nix-lambda? node) "λ "]
        [(string? node) "\"\" "]
        [(number? node) "# "]
        [(boolean? node) "? "]
        [(nix-var? node) "→ "]
        [else "• "]))
    (define label
      (cond
        [(nix-let? node) (string-append type-icon base-label)]
        [(nix-lambda? node) (format "~a~a ({ ~a }:)" type-icon base-label (string-join (nix-lambda-args node) ", "))]
        [is-container? (string-append type-icon base-label)]
        [else (string-append type-icon base-label)]))
    
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
    (send whole-source-text insert code)

    ;; Mark as modified when content changes
    (mark-modified!))

  (refresh-views-only)
  (update-status-bar)
  (is-refreshing? #f))

;; Register the refresh callback in actions.rkt
(refresh-callback refresh-gui)

;; Initialize
(refresh-gui)
(send frame show #t)