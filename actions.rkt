#lang racket/gui

(require "manager.rkt")
(require "structs.rkt")
(require "compiler.rkt")
(require "parser.rkt")

(provide (all-defined-out))

;; These will be initialized by gui.rkt
(define main-frame (make-parameter #f))
(define refresh-callback (make-parameter (lambda () (void))))
(define whole-source-widget (make-parameter #f))
(define current-sourcemap (make-parameter '()))

(define (handle-error thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (message-box "Error" (exn-message e) (main-frame) '(ok stop)))])
    (thunk)))

(define (add-child-handler type)
  (handle-error
   (lambda ()
     (define s (current-session))
     (define root (editor-state-root s))
     (define path (editor-state-path s))
     (define current-node (get-node root path))
     
     (define needs-key? (not (nix-list? current-node)))
     (define key (if needs-key? (get-text-from-user "New Child" "Enter name:" (main-frame)) #f))
     
     (when (or (not needs-key?) (and key (non-empty-string? key)))
       (define val
         (match type
           ["set" (nix-set '())]
           ["list" (nix-list '())]
           ["let" (nix-let '() (nix-set '()))]
           ["lambda"
            (let ([args-str (get-text-from-user "New Lambda" "Enter arguments (comma separated):" (main-frame))])
              (if args-str
                  (let ([args (map string-trim (string-split args-str ","))])
                    (nix-lambda args (nix-set '())))
                  #f))]
           ["value" 
            (let ([v (get-text-from-user "New Value" "Enter value (e.g. \"str\", 1, true):" (main-frame))])
              (if v (parse-value v) #f))]))
       
       (when val
         (push-history!)
         (if (nix-list? current-node) (push! val) (set-val! key val))
         ((refresh-callback)))))))

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
       (define new-key (get-text-from-user "Rename" "New name:" (main-frame) current-key))
       (when (and new-key (not (equal? new-key current-key)) (non-empty-string? new-key))
         (up!)
         (rename-child! current-key new-key)
         (enter! new-key)))
     
     ;; 2. Update Value if Scalar
     (define node (get-node (editor-state-root (current-session)) (editor-state-path (current-session))))
     (unless (or (nix-set? node) (nix-list? node) (nix-let? node))
       (define v-str (to-nix node))
       (define new-v (get-text-from-user "Update Value" "New value expression:" (main-frame) v-str))
       (when new-v (set-val! (parse-value new-v))))
     
     ((refresh-callback)))))

(define (comment-handler)
  (handle-error
   (lambda ()
     (define v (get-text-from-user "Comment" "Enter comment text:" (main-frame)))
     (when (and v (non-empty-string? v))
       (push-history!)
       (annotate! v)
       ((refresh-callback))))))

(define (delete-selected-handler)
  (handle-error
   (lambda ()
     (define path (editor-state-path (current-session)))
     (when (empty? path) (error "Cannot delete root"))
     (when (equal? (message-box "Confirm" "Delete selected node?" (main-frame) '(yes-no caution)) 'yes)
       (push-history!)
       (define key (get-current-key))
       (up!)
       (delete-child! key)
       ((refresh-callback))))))

(define (wrap-in-scope-handler)
  (handle-error
   (lambda ()
     (push-history!)
     (let* ([s (current-session)]
            [path (editor-state-path s)]
            [node (get-node (editor-state-root s) path)])
       (set-val! (nix-let '() node))
       ((refresh-callback))))))

(define (unwrap-handler)
  (handle-error
   (lambda ()
     (let* ([s (current-session)]
            [path (editor-state-path s)]
            [node (get-node (editor-state-root s) path)])
       (cond
         [(nix-let? node)
          (begin
            (push-history!)
            (set-val! (nix-let-body node))
            ((refresh-callback)))]
         [(nix-lambda? node)
          (begin
            (push-history!)
            (set-val! (nix-lambda-body node))
            ((refresh-callback)))]
         [else (error "Selected node is not a scope (let) or lambda block")])))))

(define (wrap-in-lambda-handler)
  (handle-error
   (lambda ()
     (define args-str (get-text-from-user "Wrap in Lambda" "Enter arguments (comma separated):" (main-frame)))
     (when args-str
       (push-history!)
       (let* ([s (current-session)]
              [path (editor-state-path s)]
              [node (get-node (editor-state-root s) path)]
              [args (map string-trim (string-split args-str ","))])
         (set-val! (nix-lambda args node))
         ((refresh-callback)))))))

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
      ((refresh-callback) #f))))

(define (handle-auto-update)
  (when (whole-source-widget)  ;; Guard against calls during initialization
    (define raw (send (whole-source-widget) get-text))
    (with-handlers ([exn:fail? (lambda (e) (void))]) ;; Silent: retain last tree on syntax error
    (define val (parse-nix-structure-strict raw))
    (let ([s (current-session)])
      (current-session (struct-copy editor-state s [root val])))
    ;; Regenerate map for cursor sync
    (define-values (code mapping) (to-nix-mapped (editor-state-root (current-session))))
    (current-sourcemap mapping)
    ((refresh-callback) #f)))) ;; pass #f to indicate we don't want to refresh the source text itself if possible?

