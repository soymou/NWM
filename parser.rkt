#lang racket

(require "structs.rkt")
(require "manager.rkt")

(provide parse-nix-structure-strict parse-nix-structure)

(define (parse-nix-structure-strict str)
  ;; Tokenizer: matches braces, brackets, operators, strings, and identifiers
  (define tokens
    (regexp-match* #px"\\{|\\}|\\[|\\]|=|:|;|\\\"[^\\\"]*\\\"|[^\\s\\{\\}\\[\\]=:;]+" str))
  
  (when (or (not tokens) (empty? tokens)) (error "Empty input"))
  
  (define (parse-expr toks)
    (match toks
      ['() (error "Unexpected EOF")]
      [(cons "{" rest)
       ;; Lookahead for Lambda or Set
       (let ([maybe-lambda (regexp-match? #px"^\\s*[^:]+:" (string-join rest " "))])
         (if (and (member ":" rest) maybe-lambda)
             (parse-lambda toks)
             (parse-set rest))) ]
      [(cons "[" rest) (parse-list rest)]
      [(cons "let" rest) (parse-let rest)]
      [(cons t rest) (values (parse-value t) rest)]))
  
  (define (parse-lambda toks)
    ;; toks starts with "{"
    (let loop ([ts (rest toks)] [args '()])
      (match ts
        [(cons "}" (cons ":" rest))
         (let-values ([(body after-body) (parse-expr rest)])
           (values (nix-lambda (reverse args) body) after-body))]
        [(cons "," rest) (loop rest args)]
        [(cons "..." rest) (loop rest args)]
        [(cons arg rest) (loop rest (cons arg args))]
        [_ (error "Invalid lambda syntax")])))

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
             [_ (error "Expected ';'")]))]
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
             [_ (error "Expected ';'")]))]
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