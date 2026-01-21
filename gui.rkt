#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")

;; =============================================================================
;; THEME HELPERS
;; =============================================================================

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

;; =============================================================================
;; GLOBAL STATE & HELPERS
;; =============================================================================

(define current-sourcemap (make-parameter '()))
(define is-refreshing? (make-parameter #f))

(define (handle-error thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (message-box "Error" (exn-message e) #f '(ok stop)))])
    (thunk)))

;; =============================================================================
;; CUSTOM CONTROLS
;; =============================================================================

(define (handle-auto-update)
  (define raw (send whole-source-text get-text))
  (with-handlers ([exn:fail? (lambda (e) (void))]) ;; Silent: retain last tree on syntax error
    (define val (parse-nix-structure-strict raw))
    (let ([s (current-session)])
      (current-session (struct-copy editor-state s [root val])))
    ;; Regenerate map for cursor sync
    (define-values (code mapping) (to-nix-mapped (editor-state-root (current-session))))
    (current-sourcemap mapping)
    (refresh-views-only)))

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
;; =============================================================================
;; MAIN WINDOW
;; =============================================================================

(define frame (new frame%
                   [label "Nix Workspace Manager"]
                   [width 1000]
                   [height 700]))

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

;; =============================================================================
;; TOOLBAR & ACTIONS
;; =============================================================================

(define (add-child-handler type)
  (handle-error
   (lambda ()
     (define s (current-session))
     (define root (editor-state-root s))
     (define path (editor-state-path s))
     (define current-node (get-node root path))
     
     (define needs-key? (not (nix-list? current-node)))
     (define key (if needs-key? (get-text-from-user "New Child" "Enter name:" frame) #f))
     
     (when (or (not needs-key?) (and key (non-empty-string? key)))
       (define val
         (match type
           ["set" (nix-set '())]
           ["list" (nix-list '())]
           ["let" (nix-let '() (nix-set '()))]
           ["value" 
            (let ([v (get-text-from-user "New Value" "Enter value (e.g. \"str\", 1, true):" frame)])
              (if v (parse-value v) #f))]))
       
       (when val
         (push-history!)
         (if (nix-list? current-node) (push! val) (set-val! key val))
         (refresh-gui))))))

(define (update-selected-handler)
  (handle-error
   (lambda ()
     (define s (current-session))
     (define path (editor-state-path s))
     (when (empty? path) (error "Cannot update root"))
     
     (define parent-path (get-parent-path))
     (define current-key (get-current-key))
     (define parent-node (get-node (editor-state-root s) parent-path))
     
     (push-history!)
     
     ;; 1. Rename if in Set/Let
     (when (or (nix-set? parent-node) (nix-let? parent-node))
       (define new-key (get-text-from-user "Rename" "New name:" frame current-key))
       (when (and new-key (not (equal? new-key current-key)) (non-empty-string? new-key))
         (up!)
         (rename-child! current-key new-key)
         (enter! new-key)))
     
     ;; 2. Update Value if Scalar
     (define node (get-node (editor-state-root (current-session)) (editor-state-path (current-session))))
     (unless (or (nix-set? node) (nix-list? node) (nix-let? node))
       (define v-str (to-nix node))
       (define new-v (get-text-from-user "Update Value" "New value expression:" frame v-str))
       (when new-v (set-val! (parse-value new-v))))
     
     (refresh-gui))))

(define (comment-handler)
  (handle-error
   (lambda ()
     (define v (get-text-from-user "Comment" "Enter comment text:" frame))
     (when (and v (non-empty-string? v))
       (push-history!)
       (annotate! v)
       (refresh-gui)))))

(define (delete-selected-handler)
  (handle-error
   (lambda ()
     (define path (editor-state-path (current-session)))
     (when (empty? path) (error "Cannot delete root"))
     (when (equal? (message-box "Confirm" "Delete selected node?" frame '(yes-no caution)) 'yes)
       (push-history!)
       (define key (get-current-key))
       (up!)
       (delete-child! key)
       (refresh-gui)))))

(define (wrap-in-scope-handler)
  (handle-error
   (lambda ()
     (push-history!)
     (let* ([s (current-session)]
            [path (editor-state-path s)]
            [node (get-node (editor-state-root s) path)])
       (set-val! (nix-let '() node))
       (refresh-gui)))))

(define (unwrap-scope-handler)
  (handle-error
   (lambda ()
     (let* ([s (current-session)]
            [path (editor-state-path s)]
            [node (get-node (editor-state-root s) path)])
       (if (nix-let? node)
           (begin
             (push-history!)
             (set-val! (nix-let-body node))
             (refresh-gui))
           (error "Selected node is not a scope (let) block"))))))

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
(new button% [parent toolbar-panel] [label "Unwrap"] [callback (lambda (b e) (unwrap-scope-handler))])
(new button% [parent toolbar-panel] [label "Update"] [callback (lambda (b e) (update-selected-handler))])
(new button% [parent toolbar-panel] [label "Comment"] [callback (lambda (b e) (comment-handler))])
(new button% [parent toolbar-panel] [label "Delete"] [callback (lambda (b e) (delete-selected-handler))])

(new panel% [parent toolbar-panel] [min-width 10] [stretchable-width #f])

(define path-msg (new message% [parent toolbar-panel] [label "/"] [auto-resize #t] [stretchable-width #t]))

;; =============================================================================
;; LAYOUT: SPLIT PANES
;; =============================================================================

;; Main Horizontal Split
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
    (define is-container? (or (nix-set? node) (nix-list? node) (nix-let? node)))
    (define is-expanded? (hash-ref expanded-paths path #f))
    (define is-selected? (equal? path current-path))
    (define base-label (if (empty? path) "/" (last path)))
    (define label (if (nix-let? node) (string-append base-label " [let]") base-label))
    
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

;; --- RIGHT: EDITORS ---
(define right-split (new vertical-panel% [parent main-split] [stretchable-width #t]))

;; 1. Whole Source (Syncing)
(define top-right-group (new group-box-panel% [parent right-split] [label "Whole Source (Navigable)"] [stretchable-height #t]))

;; Find the tightest node (smallest range) enclosing pos
(define (sync-path-from-pos pos)
  (define map-data (current-sourcemap))
  (define best-match #f)
  (define best-len 9999999)
  
  (for ([entry map-data])
    (match-let ([(cons path (cons start end)) entry])
      (when (and (>= pos start) (<= pos end))
        (let ([len (- end start)])
          (when (< len best-len)
            (set! best-len len)
            (set! best-match path))))))
  
  (when best-match
    (unless (equal? best-match (editor-state-path (current-session)))
      (let ([s (current-session)])
        (current-session (struct-copy editor-state s [path best-match])))
      (refresh-views-only))))

(define whole-source-text (new sync-text%))
(define whole-source-canvas (new editor-canvas% 
                                 [parent top-right-group] 
                                 [editor whole-source-text]
                                 [style '(no-hscroll)]))
(setup-dark-editor whole-source-text whole-source-canvas)

;; 2. Current Node Source
(define bottom-right-group (new group-box-panel% [parent right-split] [label "Current Node Source (Read Only)"] [stretchable-height #f] [min-height 200]))

(define source-view-text (new text%))
(define source-view-canvas (new editor-canvas% 
                                [parent bottom-right-group] 
                                [editor source-view-text]
                                [style '(no-hscroll)]))
(setup-dark-editor source-view-text source-view-canvas)
(send source-view-text lock #t)

;; =============================================================================
;; SIMPLE PARSER
;; =============================================================================

(define (parse-nix-structure-strict str)
  (define tokens (regexp-match* #px"\\{|\\}|\\[|\\]|=|;|\\\"[^\\\"]*\\\"|[^\\s\\{\\}\\[\\]=;]+" str))
  (when (empty? tokens) (error "Empty input"))
  
  (define (parse-expr toks)
    (match toks
      ['() (error "Unexpected EOF")]
      [(cons "{" rest) (parse-set rest)]
      [(cons "[" rest) (parse-list rest)]
      [(cons "let" rest) (parse-let rest)]
      [(cons t rest) (values (parse-value t) rest)]))
  
  (define (parse-let toks)
    (let loop ([ts toks] [bindings '()])
      (match ts
        [(cons "in" rest)
         (let-values ([(body after-body) (parse-expr rest)])
           (values (nix-let (reverse bindings) body) after-body))]
        [(cons key (cons "=" rest))
         (let-values ([(val after-val) (parse-expr rest)])
           (match after-val
             [(cons ";" after-semi)
              (loop after-semi (cons (binding (strip-quotes key) val) bindings))]
             [_ (error "Expected ';' after let binding")]))]
        [_ (error "Invalid let syntax")])))

  (define (parse-set toks)
    (let loop ([ts toks] [bindings '()])
      (match ts
        [(cons "}" rest) (values (nix-set (reverse bindings)) rest)]
        [(cons key (cons "=" rest))
         (let-values ([(val after-val) (parse-expr rest)])
           (match after-val
             [(cons ";" after-semi)
              (loop after-semi (cons (binding (strip-quotes key) val) bindings))]
             [_ (error "Expected ';' after binding")]))]
        [_ (error "Invalid set syntax")])))
  
  (define (parse-list toks)
    (let loop ([ts toks] [elems '()])
      (match ts
        [(cons "]" rest) (values (nix-list (reverse elems)) rest)]
        [_ 
         (let-values ([(val after-val) (parse-expr ts)])
           (loop after-val (cons val elems)))])))
  
  (let-values ([(res remaining) (parse-expr tokens)])
    (if (empty? remaining) res (error "Trailing garbage"))))

(define (parse-nix-structure str)
  (with-handlers ([exn:fail? (lambda (e) (nix-var str))])
    (parse-nix-structure-strict str)))

;; =============================================================================
;; LOGIC & SYNC
;; =============================================================================

(define (refresh-views-only)
  (define s (current-session))
  (define root (editor-state-root s))
  (define path (editor-state-path s))
  (define node (get-node root path))
  
  (send source-view-text lock #f)
  (send source-view-text erase)
  (send source-view-text insert (to-nix node))
  (send source-view-text lock #t)
  
  (render-tree root path))

(define (refresh-gui)
  (is-refreshing? #t)
  
  (define s (current-session))
  (define root (editor-state-root s))
  
  ;; 1. Regenerate Whole Source & Map
  (define-values (code mapping) (to-nix-mapped root))
  (current-sourcemap mapping)
  
  (send whole-source-text erase)
  (send whole-source-text insert code)
  
  ;; 2. Refresh other views
  (refresh-views-only)
  
  (is-refreshing? #f))

;; Initialize
(refresh-gui)
(send frame show #t)
