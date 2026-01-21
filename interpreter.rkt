#lang racket

(require "manager.rkt")
(require "compiler.rkt")
(require "structs.rkt")

;; =============================================================================
;; 1. HELPERS
;; =============================================================================

(define (format-path path)
  (if (empty? path) "/" (string-append "/" (string-join (map ~a path) "/"))))

;; --- ROBUST PARSER ---
;; Handles quotes correctly: set desc "my project" -> '("set" "desc" "\"my project\"")
(define (parse-line line)
  (define tokens 
    (regexp-match* #px"[^\\s\"]+|\"[^\"]*\"" line))
  tokens)

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

       [else (nix-var "<leaf>")])]))

(define (list-node-children node)
  (match node
    [(struct nix-set (bindings))
     (map binding-name bindings)]
    [(struct nix-list (elems))
     (build-list (length elems) number->string)]
    [(struct nix-let (bindings body))
     '("bindings" "body")]
    [else '()]))

;; =============================================================================
;; 2. EXECUTION ENGINE
;; =============================================================================

(define (execute command)
  (match command
    ;; --- BUFFER MANAGEMENT ---
    [(list "edit" filename)
     (switch-buffer! filename)
     (printf "-> Switched to buffer: [~a]\n" filename)]

    [(or (list "ls" "files") (list "buffers"))
     (displayln "--- Open Buffers ---")
     (for ([b (get-buffer-list)])
       (printf " ~a ~a\n" 
               (if (equal? b (current-buffer-name)) "*" " ") 
               b))]
    
    [(or (list "set-root" type) (list "mkfile" type))
     (push-history!)
     (reset-to-type! type)
     (printf "-> Buffer reset to empty ~a.\n" type)]

    ;; --- NAVIGATION ---
    [(list "cd" raw-path)
     (let ([path (strip-quotes raw-path)])
       (cond
         [(equal? path "..") (execute '("back"))]
         [(equal? path "/")  (execute '("top"))]
         [else
          (let* ([s (current-session)]
                 [target (append (editor-state-path s) (list path))]
                 [node (get-node (editor-state-root s) target)])
            (match node
              [(struct nix-var (name))
               (if (string-prefix? name "<error")
                   (printf "Error: Path '~a' not found.\n" path)
                   (enter! path))]
              [else (enter! path)]))]))]

    [(list "back")
     (if (empty? (editor-state-path (current-session)))
         (printf "! Root.\n")
         (up!))]

    [(list "top") (top!)]

    ;; --- STRUCTURAL EDITING ---
    [(list "mkset" raw-key)
     (let ([key (strip-quotes raw-key)])
       (push-history!)
       (set-val! key (nix-set '()))
       (printf "+ Set: ~a\n" key))]

    [(list "mklist" raw-key)
     (let ([key (strip-quotes raw-key)])
       (push-history!)
       (set-val! key (nix-list '()))
       (printf "+ List: ~a\n" key))]

    ;; NEW: Create Let Expression
    [(list "mklet")
     (push-history!)
     ;; Initialize with empty bindings and empty set body
     (set-val! (nix-let '() (nix-set '()))) 
     (printf "+ Let Block (replaced current node)\n")]

    [(list "mklet" raw-key)
     (let ([key (strip-quotes raw-key)])
       (push-history!)
       (set-val! key (nix-let '() (nix-set '()))) 
       (printf "+ Let Block at: ~a\n" key))]

    ;; --- VALUE EDITING ---
    [(list "set" raw-key raw-vals ...)
     (let ([key (strip-quotes raw-key)]
           [val (if (= (length raw-vals) 1)
                    (parse-value (first raw-vals))
                    (string-join (map strip-quotes raw-vals) " "))])
       (push-history!)
       (set-val! key val)
       (printf "✓ ~a = ~a\n" key (if (string? val) (format "\"~a\"" val) val)))]

    [(list "push" raw-vals ...)
     (let ([val (if (= (length raw-vals) 1)
                    (parse-value (first raw-vals))
                    (string-join (map strip-quotes raw-vals) " "))])
       (push-history!)
       (push! val)
       (printf "✓ Pushed: ~a\n" (if (string? val) (format "\"~a\"" val) val)))]

    [(list "comment" text ...)
     (push-history!)
     (annotate! (string-join (map strip-quotes text) " "))
     (printf "✓ Added comment.\n")]

    [(or (list "rm" raw-key) (list "delete" raw-key))
     (let ([key (strip-quotes raw-key)])
       (push-history!)
       (with-handlers ([exn:fail? (lambda (e) (printf "Error: ~a\n" (exn-message e)))])
         (delete-child! key)
         (printf "✓ Deleted: ~a\n" key)))]

    ;; --- TEMPLATES ---
    [(list "init" "flake")
     (push-history!)
     (displayln "-> Initializing Flake...")
     (execute '("mkset" "inputs"))
     (execute '("mkset" "outputs"))
     (execute '("cd" "inputs"))
     (execute '("mkset" "nixpkgs"))
     (execute '("cd" "nixpkgs"))
     (execute '("set" "url" "\"github:nixos/nixpkgs/nixos-unstable\""))
     (execute '("top"))]

    [(list "init" "shell")
     (push-history!)
     (execute '("mklist" "buildInputs"))
     (execute '("set" "shellHook" "\"echo 'Welcome'\""))]

    ;; --- SYSTEM & VIEWING ---
    [(list "ls")
     (let* ([s (current-session)]
            [node (get-node (editor-state-root s) (editor-state-path s))])
       (define children (list-node-children node))
       (if (empty? children)
           (printf "(Leaf node or empty. Use 'cat' to view content)\n")
           (for ([c children]) (displayln c))))]

    [(list "cat")
     (let* ([s (current-session)]
            [node (get-node (editor-state-root s) (editor-state-path s))])
       (displayln (to-nix node)))]

    [(list "undo") (undo!) (displayln "↺ Undone.")]
    
    [(list "clear") 
     (push-history!)
     (reset!)
     (displayln "--- State Wiped ---")]

    [(list "save")
     (let ([name (current-buffer-name)])
       (with-handlers ([exn:fail? (lambda (e) (printf "Error: ~a\n" (exn-message e)))])
         (save-config! name)
         (printf "✓ Saved [~a]\n" name)))]

    [(list "save" "as" name)
     (save-config! name)
     (switch-buffer! name)
     (printf "✓ Saved as [~a]\n" name)]

    [(list "exit") (exit)]
    
    [(list "help") 
     (displayln "Commands:")
     (displayln "  Buffers: edit <file>, ls files, set-root <set|list>, save [as], clear")
     (displayln "  Nav:     cd <path>, back, top")
     (displayln "  Create:  mkset <k>, mklist <k>, mklet [k]")
     (displayln "  Edit:    set <k> <v>, push <v>, rm <k>, comment <text>")
     (displayln "  View:    ls (children), cat (source)")
     (displayln "  Tools:   init <flake|shell>, undo")]

    [else (printf "Unknown command: ~a\n" command)]))

;; =============================================================================
;; 3. READLINE & COMPLETION
;; =============================================================================

(define readline-available? #f)
(define readline-func read-line)

(with-handlers ([exn:fail? (lambda (e) 
                             (displayln "Note: 'readline' library not found. Falling back to basic input.")
                             (void))])
  ;; Use the module that provides the standard procedural interface
  (set! readline-func (dynamic-require 'readline/readline 'readline))
  (define scf (dynamic-require 'readline/readline 'set-completion-function!))
  (set! readline-available? #t)
  
  (define commands '("edit" "set-root" "mkfile" "cd" "back" "top" "mkset" "mklist" "mklet" "set" "push" "comment" "rm" "delete" "init" "ls" "cat" "undo" "clear" "save" "exit" "help"))
  
  (scf (lambda (text)
         (filter (lambda (c) (string-prefix? c text)) commands))))

;; =============================================================================
;; 4. REPL LOOP
;; =============================================================================

(define (repl)
  (define prompt (format "[~a] ~a > " (current-buffer-name) (format-path (editor-state-path (current-session)))))
  
  (define input 
    (if readline-available?
        (readline-func prompt)
        (begin
          (display prompt)
          (flush-output)
          (read-line))))

  (unless (eof-object? input)
    (let ([raw-cmd (parse-line input)])
      (unless (empty? raw-cmd)
        (define cleaned-cmd (cons (strip-quotes (first raw-cmd)) (rest raw-cmd)))
        (with-handlers ([exn:fail? (lambda (e) (printf "Error: ~a\n" (exn-message e)))])
          (execute cleaned-cmd))))
    (repl)))

(displayln "Nix Workspace Manager v0.1")
(repl)
