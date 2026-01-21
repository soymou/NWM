#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")
(require "theme.rkt")
(require "parser.rkt")
(require "actions.rkt")

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

;; --- MAIN SPLIT ---
(define main-split (new horizontal-panel% [parent frame] [alignment '(center center)]))

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