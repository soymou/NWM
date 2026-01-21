#lang racket

(provide (all-defined-out))

;; =============================================================================
;; AST NODES FOR NIX EXPRESSIONS
;; =============================================================================

;; --- Basic Values ---
(struct nix-var (name) #:transparent)              ; identifier
(struct nix-int (value) #:transparent)             ; integer literal
(struct nix-float (value) #:transparent)           ; float literal
(struct nix-string (parts) #:transparent)          ; string (list of strings and interpolations)
(struct nix-path (value) #:transparent)            ; path literal
(struct nix-uri (value) #:transparent)             ; URI literal
(struct nix-bool (value) #:transparent)            ; true/false
(struct nix-null () #:transparent)                 ; null

;; --- Compound Types ---
(struct nix-list (elems) #:transparent)            ; [ ... ]
(struct nix-set (bindings) #:transparent)          ; { ... }
(struct nix-rec-set (bindings) #:transparent)      ; rec { ... }

;; --- Bindings ---
(struct binding (name value) #:transparent)        ; name = value;
(struct inherit-binding (names) #:transparent)     ; inherit name1 name2;
(struct inherit-from-binding (from names) #:transparent) ; inherit (expr) name1 name2;

;; --- Functions ---
(struct nix-lambda (param body) #:transparent)     ; pattern: body
(struct nix-call (func arg) #:transparent)         ; func arg

;; --- Lambda Parameters ---
(struct param-id (name) #:transparent)             ; x: body
(struct param-set (args variadic? at-name) #:transparent) ; { a, b, ... }[@name]: body
(struct param-arg (name default) #:transparent)    ; arg or arg ? default

;; --- Control Flow ---
(struct nix-if (cond then-expr else-expr) #:transparent) ; if c then t else e
(struct nix-let (bindings body) #:transparent)     ; let ... in body
(struct nix-with (env body) #:transparent)         ; with env; body
(struct nix-assert (cond body) #:transparent)      ; assert cond; body

;; --- Operators ---
(struct nix-binop (op left right) #:transparent)   ; left op right
(struct nix-unop (op arg) #:transparent)           ; op arg (!, -)
(struct nix-select (expr path default) #:transparent) ; expr.path [or default]
(struct nix-has-attr (expr path) #:transparent)    ; expr ? path

;; --- String Interpolation ---
(struct nix-interp (expr) #:transparent)           ; ${expr} inside strings

;; --- Comments (metadata) ---
(struct nix-comment (text node) #:transparent)     ; # comment attached to node

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

(define (strip-quotes s)
  (if (and (string-prefix? s "\"") (string-suffix? s "\"") (> (string-length s) 1))
      (substring s 1 (sub1 (string-length s)))
      s))

;; Convert parsed value to appropriate AST node
(define (make-literal s)
  (cond
    [(equal? s "true") (nix-bool #t)]
    [(equal? s "false") (nix-bool #f)]
    [(equal? s "null") (nix-null)]
    [(regexp-match? #px"^-?[0-9]+$" s) (nix-int (string->number s))]
    [(regexp-match? #px"^-?[0-9]+\\.[0-9]+$" s) (nix-float (string->number s))]
    [else (nix-var s)]))

;; Parse user-entered value string to AST node
(define (parse-value s)
  (cond
    [(and (string-prefix? s "\"") (string-suffix? s "\""))
     (nix-string (list (strip-quotes s)))]
    [else (make-literal s)]))
