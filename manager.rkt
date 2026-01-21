#lang racket

(require "structs.rkt")
(require "compiler.rkt")
(require "parser.rkt")

(provide (all-defined-out))

;; =============================================================================
;; SHARED HELPER FUNCTIONS
;; =============================================================================

(define (format-path path)
  (if (empty? path) "/" (string-append "/" (string-join (map ~a path) "/"))))

;; Get binding name as string (handles list attribute paths)
(define (binding-name-str b)
  (define name (binding-name b))
  (if (list? name)
      (string-join name ".")
      name))

;; Check if a binding matches a key (key may be a string or a list)
(define (binding-matches? b key)
  (define name (binding-name b))
  (cond
    ;; Both are lists - compare directly
    [(and (list? name) (list? key)) (equal? name key)]
    ;; Name is list, key is string - convert name to string
    [(list? name) (equal? (string-join (map ~a name) ".") key)]
    ;; Name is string, key is list - convert key to string
    [(list? key) (equal? name (string-join (map ~a key) "."))]
    ;; Both are strings
    [else (equal? name key)]))

;; --- NODE TRAVERSAL ---
(define (get-node root path)
  (match path
    ['() root]
    [(cons key rest)
     (match root
       ;; 1. Sets and Rec Sets
       [(or (struct nix-set (bindings)) (struct nix-rec-set (bindings)))
        (let ([b (findf (lambda (x)
                          (and (binding? x) (binding-matches? x key)))
                        bindings)])
          (if b
              (get-node (binding-value b) rest)
              (nix-var "<error: key-not-found>")))]

       ;; 2. Lists
       [(struct nix-list (elems))
        (let ([idx (string->number key)])
          (if (and (integer? idx) (< idx (length elems)) (>= idx 0))
              (get-node (list-ref elems idx) rest)
              (nix-var "<error: index-out-of-bounds>")))]

       ;; 3. Let Expressions (Virtual Directories)
       [(struct nix-let (bindings body))
        (match key
          ["bindings" (get-node (nix-set bindings) rest)]
          ["body"     (get-node body rest)]
          [else       (nix-var "<error: invalid-let-path>")])]

       ;; 4. Lambdas
       [(struct nix-lambda (param body))
        (match key
          ["body" (get-node body rest)]
          [else   (nix-var "<error: invalid-lambda-path>")])]

       ;; 5. If expressions
       [(struct nix-if (cond-e then-e else-e))
        (match key
          ["cond" (get-node cond-e rest)]
          ["then" (get-node then-e rest)]
          ["else" (get-node else-e rest)]
          [else   (nix-var "<error: invalid-if-path>")])]

       ;; 6. With expressions
       [(struct nix-with (env body))
        (match key
          ["env"  (get-node env rest)]
          ["body" (get-node body rest)]
          [else   (nix-var "<error: invalid-with-path>")])]

       ;; 7. Assert expressions
       [(struct nix-assert (cond-e body))
        (match key
          ["cond" (get-node cond-e rest)]
          ["body" (get-node body rest)]
          [else   (nix-var "<error: invalid-assert-path>")])]

       ;; 8. Binary operators
       [(struct nix-binop (op left right))
        (match key
          ["left"  (get-node left rest)]
          ["right" (get-node right rest)]
          [else    (nix-var "<error: invalid-binop-path>")])]

       ;; 9. Unary operators
       [(struct nix-unop (op arg))
        (match key
          ["arg" (get-node arg rest)]
          [else  (nix-var "<error: invalid-unop-path>")])]

       ;; 10. Function calls
       [(struct nix-call (func arg))
        (match key
          ["func" (get-node func rest)]
          ["arg"  (get-node arg rest)]
          [else   (nix-var "<error: invalid-call-path>")])]

       ;; 11. Attribute selection
       [(struct nix-select (expr attr-path default))
        (match key
          ["expr"    (get-node expr rest)]
          ["default" (if default (get-node default rest) (nix-var "<no-default>"))]
          [else      (nix-var "<error: invalid-select-path>")])]

       ;; 12. Has attribute
       [(struct nix-has-attr (expr attr-path))
        (match key
          ["expr" (get-node expr rest)]
          [else   (nix-var "<error: invalid-has-attr-path>")])]

       [else (nix-var "<leaf>")])]))

(define (list-node-children node)
  (match node
    ;; Sets
    [(or (struct nix-set (bindings)) (struct nix-rec-set (bindings)))
     (filter-map (lambda (b)
                   (cond
                     [(binding? b) (binding-name-str b)]
                     [(inherit-binding? b) #f]  ; Skip inherit for now
                     [(inherit-from-binding? b) #f]
                     [else #f]))
                 bindings)]

    ;; Lists
    [(struct nix-list (elems))
     (build-list (length elems) number->string)]

    ;; Let expressions
    [(struct nix-let (bindings body))
     '("bindings" "body")]

    ;; Lambdas
    [(struct nix-lambda (param body))
     '("body")]

    ;; If expressions
    [(struct nix-if (cond-e then-e else-e))
     '("cond" "then" "else")]

    ;; With expressions
    [(struct nix-with (env body))
     '("env" "body")]

    ;; Assert expressions
    [(struct nix-assert (cond-e body))
     '("cond" "body")]

    ;; Binary operators
    [(struct nix-binop (op left right))
     '("left" "right")]

    ;; Unary operators
    [(struct nix-unop (op arg))
     '("arg")]

    ;; Function calls
    [(struct nix-call (func arg))
     '("func" "arg")]

    ;; Attribute selection
    [(struct nix-select (expr attr-path default))
     (if default '("expr" "default") '("expr"))]

    ;; Has attribute
    [(struct nix-has-attr (expr attr-path))
     '("expr")]

    ;; Everything else has no children
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

(define (load-file! filename)
  (let* ([content (file->string filename)]
         [ast (parse-nix-structure-strict content)])
    (current-session (editor-state ast '()))
    (current-buffer-name filename)
    (open-buffers (hash-set (open-buffers) filename (current-session)))
    (history '())))

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
                   (if (and (binding? b) (binding-matches? b key))
                       (begin (set! found? #t)
                              (binding (binding-name b) (update-in (binding-value b) rest f)))
                       b))
                 bindings))
          (if found?
              (nix-set new-bindings)
              (nix-set (append bindings (list (binding key (f #f)))))))]

       ;; Rec Sets
       [(struct nix-rec-set (bindings))
        (let ([found? #f])
          (define new-bindings
            (map (lambda (b)
                   (if (and (binding? b) (binding-matches? b key))
                       (begin (set! found? #t)
                              (binding (binding-name b) (update-in (binding-value b) rest f)))
                       b))
                 bindings))
          (if found?
              (nix-rec-set new-bindings)
              (nix-rec-set (append bindings (list (binding key (f #f)))))))]

       ;; Lists
       [(struct nix-list (elems))
        (unless (number? key) (error "Index must be number"))
        (nix-list (list-set elems key (update-in (list-ref elems key) rest f)))]

       ;; Let Expressions
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
       [(struct nix-lambda (param body))
        (match key
          ["body" (nix-lambda param (update-in body rest f))]
          [else (error "Inside 'lambda', use 'cd body'")])]

       ;; If expressions
       [(struct nix-if (cond-e then-e else-e))
        (match key
          ["cond" (nix-if (update-in cond-e rest f) then-e else-e)]
          ["then" (nix-if cond-e (update-in then-e rest f) else-e)]
          ["else" (nix-if cond-e then-e (update-in else-e rest f))]
          [else (error "Inside 'if', use 'cd cond', 'cd then', or 'cd else'")])]

       ;; With expressions
       [(struct nix-with (env body))
        (match key
          ["env"  (nix-with (update-in env rest f) body)]
          ["body" (nix-with env (update-in body rest f))]
          [else (error "Inside 'with', use 'cd env' or 'cd body'")])]

       ;; Assert expressions
       [(struct nix-assert (cond-e body))
        (match key
          ["cond" (nix-assert (update-in cond-e rest f) body)]
          ["body" (nix-assert cond-e (update-in body rest f))]
          [else (error "Inside 'assert', use 'cd cond' or 'cd body'")])]

       ;; Binary operators
       [(struct nix-binop (op left right))
        (match key
          ["left"  (nix-binop op (update-in left rest f) right)]
          ["right" (nix-binop op left (update-in right rest f))]
          [else (error "Inside binary op, use 'cd left' or 'cd right'")])]

       ;; Unary operators
       [(struct nix-unop (op arg))
        (match key
          ["arg" (nix-unop op (update-in arg rest f))]
          [else (error "Inside unary op, use 'cd arg'")])]

       ;; Function calls
       [(struct nix-call (func arg))
        (match key
          ["func" (nix-call (update-in func rest f) arg)]
          ["arg"  (nix-call func (update-in arg rest f))]
          [else (error "Inside call, use 'cd func' or 'cd arg'")])]

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
                                                        (if (and (binding? b) (binding-matches? b old-key))
                                                            (binding new-key (binding-value b))
                                                            b))
                                                      bindings))]
                                       [(struct nix-rec-set (bindings))
                                        (nix-rec-set (map (lambda (b)
                                                            (if (and (binding? b) (binding-matches? b old-key))
                                                                (binding new-key (binding-value b))
                                                                b))
                                                          bindings))]
                                       [(struct nix-let (bindings body))
                                        (nix-let (map (lambda (b)
                                                        (if (and (binding? b) (binding-matches? b old-key))
                                                            (binding new-key (binding-value b))
                                                            b))
                                                      bindings) body)]
                                       [else (error "Focus is not a Set, Rec Set, or Let bindings")])))]))))

(define set-val!
  (case-lambda
    ;; CASE 1: One Argument
    ;; Replace the node at the current path with 'val'
    [(val)
     (let ([s (current-session)])
       (current-session
        (struct-copy editor-state s
                     [root (update-in (editor-state-root s)
                                      (editor-state-path s)
                                      (lambda (_) val))])))]

    ;; CASE 2: Two Arguments
    ;; Insert or update 'key' with 'val' inside the current Set
    [(key val)
     (let ([s (current-session)])
       (current-session
        (struct-copy editor-state s
                     [root (update-in (editor-state-root s)
                                      (editor-state-path s)
                                      (lambda (n)
                                        (match n
                                          [(struct nix-set (bindings))
                                           (let ([clean (filter (lambda (b)
                                                                  (not (and (binding? b) (binding-matches? b key))))
                                                                bindings)])
                                             (nix-set (append clean (list (binding key val)))))]
                                          [(struct nix-rec-set (bindings))
                                           (let ([clean (filter (lambda (b)
                                                                  (not (and (binding? b) (binding-matches? b key))))
                                                                bindings)])
                                             (nix-rec-set (append clean (list (binding key val)))))]
                                          [(struct nix-let (bindings body))
                                           (cond
                                             [(equal? key "body") (nix-let bindings val)]
                                             [(equal? key "bindings") (error "Cannot overwrite bindings container")]
                                             [else
                                              (let ([clean (filter (lambda (b)
                                                                     (not (and (binding? b) (binding-matches? b key))))
                                                                   bindings)])
                                                (nix-let (append clean (list (binding key val))) body))])]
                                          [#f val]
                                          [else (error "focus is not a set, rec set, or let")])))])))]))

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
                                        (nix-set (filter (lambda (b)
                                                           (not (and (binding? b) (binding-matches? b key))))
                                                         bindings))]
                                       [(struct nix-rec-set (bindings))
                                        (nix-rec-set (filter (lambda (b)
                                                               (not (and (binding? b) (binding-matches? b key))))
                                                             bindings))]
                                       [(struct nix-list (elems))
                                        (let ([idx (string->number key)])
                                          (if (and (integer? idx) (>= idx 0) (< idx (length elems)))
                                              (nix-list (append (take elems idx) (drop elems (add1 idx))))
                                              (error "Index out of bounds")))]
                                       [(struct nix-let (bindings body))
                                        (cond
                                          [(member key '("bindings" "body")) (error "Cannot delete structural let elements")]
                                          [else (nix-let (filter (lambda (b)
                                                                   (not (and (binding? b) (binding-matches? b key))))
                                                                 bindings) body)])]
                                       [else (error "Current node cannot have children deleted")])))]))))
