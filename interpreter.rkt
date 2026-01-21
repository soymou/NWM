#lang racket

(require "manager.rkt")
(require "compiler.rkt")
(require "structs.rkt")

;; =============================================================================
;; 1. HELPERS
;; =============================================================================

;; --- ROBUST PARSER ---
;; Handles quotes correctly: set desc "my project" -> '("set" "desc" "\"my project\"")
(define (parse-line line)
  (define tokens 
    (regexp-match* #px"[^\\s\"]+|\"[^\"]*\"" line))
  tokens)

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
