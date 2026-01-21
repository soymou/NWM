#lang racket

(provide (all-defined-out))

;; --- AST NODES ---

;; A Binding links a Name to a Value (e.g. url = "...")
(struct binding (name value) #:transparent)

;; A Set is a collection of bindings { ... }
(struct nix-set (bindings) #:transparent)

;; A List is an ordered sequence [ ... ]
(struct nix-list (elems) #:transparent)

;; A Function Call (e.g. pkgs.mkShell ...)
(struct nix-call (func arg) #:transparent)

;; A Raw Variable/Identifier (e.g. self, pkgs)
(struct nix-var (name) #:transparent)

;; A Lambda Function (e.g. { self, ... }: ...)
(struct nix-lambda (args body) #:transparent)

;; A Comment Wrapper (Metadata)
(struct nix-comment (text node) #:transparent)

;; Add this struct
(struct nix-let (bindings body) #:transparent)
