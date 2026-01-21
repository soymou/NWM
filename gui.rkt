#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")
(require mrlib/hierlist)

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
                                   (refresh-gui)))))])
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
;; CUSTOM CONTROLS
;; =============================================================================

(define dark-input-field%
  (class editor-canvas%
    (init callback [init-value ""])
    (define notify-callback callback)
    (super-new [style '(no-hscroll)] [min-height 25] [stretchable-height #f])
    
    (define text-editor 
      (new (class text% 
             (super-new)
             (define/override (on-char event)
               (define key (send event get-key-code))
               (cond
                 [(eq? key #\return) (notify-callback this event)]
                 ;; Ignore newline insertion?
                 [else (super on-char event)])))))
    
    (send this set-editor text-editor)
    (setup-dark-editor text-editor this)
    (send text-editor insert init-value)
    
    (define/public (get-value) (send text-editor get-text))
    (define/public (set-value v) 
      (send text-editor erase) 
      (send text-editor insert v))))

;; =============================================================================
;; LAYOUT: SPLIT PANES
;; =============================================================================

;; Main Horizontal Split
(define main-split (new horizontal-panel% [parent frame] [alignment '(center center)]))

;; --- LEFT: TREE VIEW ---
(define left-panel (new vertical-panel% [parent main-split] [min-width 250] [stretchable-width #f] [spacing 0] [border 0]))
(new message% [parent left-panel] [label "Structure Tree"])

;; Map to store item -> path association
(define tree-items (make-hash))
(define (tree-item-map-path! item path)
  (hash-set! tree-items item path))

(define (get-path-for-item item)
  (hash-ref tree-items item #f))

;; Custom Tree Class to handle selection
(define my-tree% 
  (class hierarchical-list%
    (super-new)
    (define/override (on-select item)
      (super on-select item)
      (unless (is-refreshing?)
        (define p (get-path-for-item item))
        (when p
          (handle-error 
           (lambda ()
             (let ([s (current-session)])
               (current-session (struct-copy editor-state s [path p])))
             (refresh-views-only))))))))

(define tree-view (new my-tree% 
                       [parent left-panel]
                       [style '(auto-vscroll auto-hscroll)]
                       [stretchable-width #t]
                       [stretchable-height #t]))
(setup-dark-editor (send tree-view get-editor) tree-view)

;; --- RIGHT: EDITORS ---
(define right-split (new vertical-panel% [parent main-split] [stretchable-width #t]))

;; 1. Whole Source (Syncing)
(define top-right-group (new group-box-panel% [parent right-split] [label "Whole Source (Navigable)"] [stretchable-height #t]))

(define sync-text% 
  (class text%
    (super-new)
    (define/override (set-position start [end 'same] [scroll #t] [select #f])
      (super set-position start end scroll select)
      (unless (is-refreshing?)
        (sync-path-from-pos start)))))

(define whole-source-text (new sync-text%))
(define whole-source-canvas (new editor-canvas% 
                                 [parent top-right-group] 
                                 [editor whole-source-text]
                                 [style '(no-hscroll)]))
(setup-dark-editor whole-source-text whole-source-canvas)

;; 2. Current Node Source
(define bottom-right-group (new group-box-panel% [parent right-split] [label "Current Node Source"] [stretchable-height #f] [min-height 200]))

(define source-view-text (new text%))
(define source-view-canvas (new editor-canvas% 
                                [parent bottom-right-group] 
                                [editor source-view-text]
                                [style '(no-hscroll)]))
(setup-dark-editor source-view-text source-view-canvas)

;; =============================================================================
;; SIMPLE PARSER
;; =============================================================================

(define (parse-nix-structure str)
  (with-handlers ([exn:fail? (lambda (e) 
                               (nix-var str))])
    (define tokens (regexp-match* #px"\\{|\\}|\\[|\\]|=|;|\\\"[^\\\"]*\\\"|[^\\s\\{\\}\\[\\]=;]+" str))
    
    (define (parse-expr toks)
      (match toks
        ['() (error "Unexpected EOF")]
        [(cons "{" rest) (parse-set rest)]
        [(cons "[" rest) (parse-list rest)]
        [(cons t rest) 
         (values (parse-value t) rest)]))
    
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
      (if (empty? remaining) res (nix-var str)))))

(define (save-manual-edit)
  (handle-error 
   (lambda ()
     (define raw (send source-view-text get-text))
     (push-history!)
     (define val (parse-nix-structure raw))
     
     ;; Check for fallback
     (define proceed? #t)
     (when (and (nix-var? val) (regexp-match? #px"^\\s*[\\{\\[]" raw))
       (set! proceed? 
             (equal? (message-box "Syntax Warning" 
                                  "The structure could not be parsed (missing ';' or closing brace?).\nSave as raw text?" 
                                  frame '(yes-no caution)) 
                     'yes)))
     
     (when proceed?
       (set-val! val)
       (refresh-gui)))))

(new button% [parent bottom-right-group] 
     [label "Apply Manual Changes"]
     [callback (lambda (b e) (save-manual-edit))])

;; =============================================================================
;; BOTTOM PANEL (CONTROLS)
;; ...

(define controls-panel (new vertical-panel% [parent frame] [stretchable-height #f]))

;; --- ROW 1: CREATION ---
(define row1 (new horizontal-panel% [parent controls-panel] [alignment '(left center)]))

(define (add-child-handler type)
  (handle-error
   (lambda ()
     (define s (current-session))
     (define root (editor-state-root s))
     (define path (editor-state-path s))
     (define current-node (get-node root path))
     
     ;; Determine if we need a key
     (define needs-key? 
       (not (nix-list? current-node)))
     
     (define key
       (if needs-key?
           (get-text-from-user "New Child" "Enter name for new key:" frame)
           #f))
     
     (when (or (not needs-key?) (and key (non-empty-string? key)))
       (push-history!)
       
       (define val
         (match type
           ["set" (nix-set '())]
           ["list" (nix-list '())]
           ["let" (nix-let '() (nix-set '()))]
           ["value" 
            (let ([v (get-text-from-user "New Value" "Enter value (e.g. \"str\", 1, true):" frame)])
              (if v (parse-value v) (error "Value cancelled")))]))
       
       (if (nix-list? current-node)
           (push! val)
           (set-val! key val))
       
       (refresh-gui)))))

(new button% [parent row1] [label "Add Child..."]
     [callback (lambda (b e)
                 (define menu (new popup-menu%))
                 (new menu-item% [parent menu] [label "Set"]
                      [callback (lambda (i e) (add-child-handler "set"))])
                 (new menu-item% [parent menu] [label "List"]
                      [callback (lambda (i e) (add-child-handler "list"))])
                 (new menu-item% [parent menu] [label "Scope (Let)"]
                      [callback (lambda (i e) (add-child-handler "let"))])
                 (new separator-menu-item% [parent menu])
                 (new menu-item% [parent menu] [label "Value (Scalar)"]
                      [callback (lambda (i e) (add-child-handler "value"))])
                 (send b popup-menu menu 0 0))])

;; --- ROW 2: EDITING ---
(define row2 (new horizontal-panel% [parent controls-panel] [alignment '(left center)]))
(new message% [parent row2] [label "Edit Value: "])

(define (do-update-current)
  (handle-error (lambda ()
                  (define v-raw (send val-field get-value))
                  (push-history!)
                  (set-val! (parse-value v-raw))
                  (refresh-gui))))

(define val-field (new dark-input-field% 
                       [parent row2]
                       [init-value ""]
                       [callback (lambda (c e) (do-update-current))]))

(new button% [parent row2] [label "Update Current"]
     [callback (lambda (b e) (do-update-current))])

(new button% [parent row2] [label "Comment"]
     [callback (lambda (b e)
                 (handle-error (lambda ()
                                 (define v-raw (send val-field get-value))
                                 (unless (non-empty-string? v-raw) (error "Comment text required"))
                                 (push-history!)
                                 (annotate! v-raw)
                                 (refresh-gui))))])

(new button% [parent row2] [label "Delete Child..."]
     [callback (lambda (b e)
                 (handle-error (lambda ()
                                 (define k (get-text-from-user "Delete" "Enter Key/Index to delete:" frame))
                                 (when (and k (non-empty-string? k))
                                   (push-history!)
                                   (delete-child! k)
                                   (refresh-gui)))))])

;; =============================================================================
;; LOGIC & SYNC
;; =============================================================================

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
    ;; Update session path without triggering refresh loop?
    ;; If we update session path, we should usually refresh GUI to update tree/node view
    ;; But we don't want to re-set the whole source text cursor.
    (unless (equal? best-match (editor-state-path (current-session)))
      ;; Update path directly
      (let ([s (current-session)])
        (current-session (struct-copy editor-state s [path best-match])))
      ;; Refresh only the views (not the whole source text)
      (refresh-views-only))))

(define (refresh-views-only)
  ;; Update Current Node Source
  (define s (current-session))
  (define root (editor-state-root s))
  (define path (editor-state-path s))
  
  (define node (get-node root path))
  (send source-view-text erase)
  (send source-view-text insert (to-nix node))
  
  ;; Update Tree Selection (Visual only)
  ;; Walking the tree to find the item is hard because hierarchical-list doesn't expose a simple "find by data".
  ;; We might have to rebuild or store references. Rebuilding is safest for now.
  (rebuild-tree root path))

(define (rebuild-tree root current-path)
  (hash-clear! tree-items)
  ;; Clear existing items? hierarchical-list doesn't seem to have 'clear'.
  ;; We delete all children.
  (for ([i (in-list (send tree-view get-items))])
    (send tree-view delete-item i))

  ;; Recursive builder
  (define (build-node parent-item node node-path)
    (define label 
      (if (empty? node-path) "/" (last node-path)))
    
    (define is-container? (or (nix-set? node) (nix-list? node) (nix-let? node)))
    
    (define item 
      (if parent-item
          (if is-container? (send parent-item new-list) (send parent-item new-item))
          (if is-container? (send tree-view new-list) (send tree-view new-item))))
    
    (define editor (send item get-editor))
    (setup-dark-text editor)
    (send editor insert (format "~a" label))
    
    ;; Highlight if selected
    (when (equal? node-path current-path)
      (send item select #t))
    
    ;; Set data for click handling
    (tree-item-map-path! item node-path)
    
    (when is-container?
      (send item open))
    
    ;; Recurse
    (match node
      [(struct nix-set (bindings))
       (for ([b bindings])
         (build-node item (binding-value b) (append node-path (list (binding-name b)))))]
      [(struct nix-list (elems))
       (for ([e elems] [i (in-naturals)])
         (build-node item e (append node-path (list (number->string i)))))]
      [(struct nix-let (bindings body))
       (build-node item (nix-set bindings) (append node-path (list "bindings")))
       (build-node item body (append node-path (list "body")))]
      [else (void)]))
  
  (build-node #f root '()))

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
