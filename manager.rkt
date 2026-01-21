#lang racket

(require "structs.rkt")
(require "compiler.rkt")

(provide (all-defined-out))

;; --- DATA STRUCTURES ---
(struct editor-state (root path) #:transparent)

;; --- 1. BUFFER SYSTEM ---
(define open-buffers (make-parameter (hash "default.nix" (editor-state (nix-set '()) '()))))
(define current-buffer-name (make-parameter "default.nix"))

;; The active session
(define current-session (make-parameter (editor-state (nix-set '()) '())))

(define (switch-buffer! name)
  (let ([registry (open-buffers)]
        [old-name (current-buffer-name)]
        [work (current-session)])
    (open-buffers (hash-set registry old-name work)))
  
  (let* ([registry (open-buffers)]
         [new-state (hash-ref registry name (thunk (editor-state (nix-set '()) '())))])
    (current-session new-state)
    (current-buffer-name name)
    (history '())))

(define (get-buffer-list)
  (hash-keys (open-buffers)))

;; --- 2. HISTORY (UNDO) ---
(define history (make-parameter '()))

(define (push-history!)
  (history (cons (current-session) (history))))

(define (undo!)
  (unless (empty? (history))
    (current-session (first (history)))
    (history (rest (history)))))

;; --- 3. IO OPERATIONS ---
(define (save-config! filename)
  (let* ([s (current-session)]
         [code (to-nix (editor-state-root s))])
    (with-output-to-file filename
      (lambda () (display code))
      #:exists 'replace)))

(define (reset!)
  (current-session (editor-state (nix-set '()) '())))

(define (reset-to-type! type)
  (match type
    ["set"  (current-session (editor-state (nix-set '()) '()))]
    ["list" (current-session (editor-state (nix-list '()) '()))]
    [else   (void)]))

;; --- 4. NAVIGATION & EDITING ---

(define (update-in node path f)
  (match path
    ['() (f node)]
    [(cons key rest)
     (match node
       ;; Sets
       [(struct nix-set (bindings))
        (let ([found? #f])
          (define new-bindings
            (map (lambda (b)
                   (if (equal? (binding-name b) key)
                       (begin (set! found? #t)
                              (binding key (update-in (binding-value b) rest f)))
                       b))
                 bindings))
          (if found?
              (nix-set new-bindings)
              (nix-set (append bindings (list (binding key (f #f)))))))] 
       
       ;; Lists
       [(struct nix-list (elems))
        (unless (number? key) (error "Index must be number"))
        (nix-list (list-set elems key (update-in (list-ref elems key) rest f)))]
       
       ;; Let Expressions (NEW)
       [(struct nix-let (bindings body))
        (match key
          ["bindings"
           (let* ([temp-set (nix-set bindings)]
                  [updated-set (update-in temp-set rest f)])
             (nix-let (nix-set-bindings updated-set) body))]
          ["body"
           (nix-let bindings (update-in body rest f))]
          [else (error "Inside 'let', use 'cd bindings' or 'cd body'")])]

       [else (error "Cannot traverse:" node)])]))

(define (enter! key)
  (let* ([s (current-session)]
         [p (editor-state-path s)])
    (current-session (struct-copy editor-state s [path (append p (list key))]))))

(define (up!)
  (let* ([s (current-session)]
         [p (editor-state-path s)])
    (unless (empty? p)
      (current-session (struct-copy editor-state s [path (drop-right p 1)])))))

(define (top!)
  (let ([s (current-session)])
    (current-session (struct-copy editor-state s [path '()]))))

(define set-val!
  (case-lambda
    ;; CASE 1: One Argument
    ;; Replace the node at the current path with 'val'
    ;; Usage: (set-val! (nix-let ...))
    [(val)
     (let ([s (current-session)])
       (current-session 
        (struct-copy editor-state s 
                     [root (update-in (editor-state-root s) 
                                      (editor-state-path s) 
                                      (lambda (_) val))]))) ]

    ;; CASE 2: Two Arguments (Your original logic)
    ;; Insert or update 'key' with 'val' inside the current Set
    ;; Usage: (set-val! "myKey" (nix-int 10))
    [(key val)
     (let ([s (current-session)])
       (current-session 
        (struct-copy editor-state s 
                     [root (update-in (editor-state-root s) 
                                      (editor-state-path s) 
                                      (lambda (n)
                                        (match n
                                          [(struct nix-set (bindings))
                                           (let ([clean (filter (lambda (b) (not (equal? (binding-name b) key))) bindings)])
                                             (nix-set (append clean (list (binding key val)))))]
                                          [#f val]
                                          [else (error "focus is not a set")])))])))]))

(define (push! val)
  (let ([s (current-session)])
    (current-session 
     (struct-copy editor-state s 
                  [root (update-in (editor-state-root s) (editor-state-path s)
                                   (lambda (n)
                                     (match n
                                       [(struct nix-list (elems))
                                        (nix-list (append elems (list val)))]
                                       [else (error "Focus is not a List")])))]))))

(define (annotate! text)
  (let ([s (current-session)])
    (current-session 
     (struct-copy editor-state s 
                  [root (update-in (editor-state-root s) (editor-state-path s)
                                   (lambda (n) (nix-comment text n)))]))))

(define (delete-child! key)
  (let ([s (current-session)])
    (current-session
     (struct-copy editor-state s
                  [root (update-in (editor-state-root s)
                                   (editor-state-path s)
                                   (lambda (n)
                                     (match n
                                       [(struct nix-set (bindings))
                                        (nix-set (filter (lambda (b) (not (equal? (binding-name b) key))) bindings))]
                                       [(struct nix-list (elems))
                                        (let ([idx (string->number key)])
                                          (if (and (integer? idx) (>= idx 0) (< idx (length elems)))
                                              (nix-list (append (take elems idx) (drop elems (add1 idx))))
                                              (error "Index out of bounds")))]
                                       [else (error "Current node cannot have children deleted")])))]))))
