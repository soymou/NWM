#lang racket

(require "structs.rkt")
(require "compiler.rkt")

(provide (all-defined-out))

;; =============================================================================
;; SHARED HELPER FUNCTIONS (Moved from interpreter.rkt)
;; =============================================================================

(define (format-path path)
  (if (empty? path) "/" (string-append "/" (string-join (map ~a path) "/"))))

(define (strip-quotes s)
  (if (and (string-prefix? s "\"") (string-suffix? s "\"") (> (string-length s) 1))
      (substring s 1 (sub1 (string-length s)))
      s))

(define (parse-value s)
  (cond
    [(regexp-match? #px"^\".*\"$" s) (strip-quotes s)] ;; Explicit string
    [(regexp-match? #px"^-?[0-9]+(\\.[0-9]+)?$" s) (string->number s)] ;; Number
    [(equal? s "true") #t]
    [(equal? s "false") #f]
    [else (nix-var s)])) ;; Raw identifier/expression

;; --- NODE TRAVERSAL ---
(define (get-node root path)
  (match path
    ['() root]
    [(cons key rest)
     (match root
       ;; 1. Sets
       [(struct nix-set (bindings))
        (let ([b (findf (lambda (x) (equal? (binding-name x) key)) bindings)])
          (if b (get-node (binding-value b) rest) (nix-var "<error: key-not-found>")))]
       
       ;; 2. Lists
       [(struct nix-list (elems))
        (let ([idx (string->number key)])
          (if (and (integer? idx) (< idx (length elems)) (>= idx 0))
              (get-node (list-ref elems idx) rest)
              (nix-var "<error: index-out-of-bounds>")))]
       
       ;; 3. Let Expressions (Virtual Directories)
       [(struct nix-let (bindings body))
        (match key
          ["bindings" (get-node (nix-set bindings) rest)] ;; Treat as Set
          ["body"     (get-node body rest)]
          [else       (nix-var "<error: invalid-let-path>")])]
       
       ;; 4. Lambdas
       [(struct nix-lambda (args body))
        (match key
          ["body" (get-node body rest)]
          [else   (nix-var "<error: invalid-lambda-path>")])]

       [else (nix-var "<leaf>")])]))

(define (list-node-children node)
  (match node
    [(struct nix-set (bindings))
     (map binding-name bindings)]
    [(struct nix-list (elems))
     (build-list (length elems) number->string)]
    [(struct nix-let (bindings body))
     '("bindings" "body")]
    [(struct nix-lambda (args body))
     '("body")]
    [else '()]))

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
    ["let"  (current-session (editor-state (nix-let '() (nix-set '())) '()))]
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
       
       ;; Lambdas
       [(struct nix-lambda (args body))
        (match key
          ["body" (nix-lambda args (update-in body rest f))]
          [else (error "Inside 'lambda', use 'cd body'")])]

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

(define (get-current-key)
  (let ([p (editor-state-path (current-session))])
    (if (empty? p) #f (last p))))

(define (get-parent-path)
  (let ([p (editor-state-path (current-session))])
    (if (empty? p) #f (drop-right p 1))))

(define (rename-child! old-key new-key)
  (let ([s (current-session)])
    (current-session 
     (struct-copy editor-state s 
                  [root (update-in (editor-state-root s) (editor-state-path s)
                                   (lambda (n)
                                     (match n
                                       [(struct nix-set (bindings))
                                        (nix-set (map (lambda (b) 
                                                        (if (equal? (binding-name b) old-key)
                                                            (binding new-key (binding-value b))
                                                            b))
                                                      bindings))]
                                       [(struct nix-let (bindings body))
                                        (nix-let (map (lambda (b) 
                                                        (if (equal? (binding-name b) old-key)
                                                            (binding new-key (binding-value b))
                                                            b))
                                                      bindings) body)]
                                       [else (error "Focus is not a Set or Let bindings")])))]))))

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
                                          [(struct nix-let (bindings body))
                                           (cond
                                             [(equal? key "body") (nix-let bindings val)]
                                             [(equal? key "bindings") (error "Cannot overwrite bindings container")]
                                             [else
                                              (let ([clean (filter (lambda (b) (not (equal? (binding-name b) key))) bindings)])
                                                (nix-let (append clean (list (binding key val))) body))])]
                                          [#f val]
                                          [else (error "focus is not a set or let")])))])))]))

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
                                       [(struct nix-let (bindings body))
                                        (cond
                                          [(member key '("bindings" "body")) (error "Cannot delete structural let elements")]
                                          [else (nix-let (filter (lambda (b) (not (equal? (binding-name b) key))) bindings) body)])]
                                       [else (error "Current node cannot have children deleted")])))]))))
