#lang racket

(require "structs.rkt")
(provide to-nix)

(define (indent n) (make-string (* n 2) #\space))

(define (to-nix expr [level 0])
  (match expr
    ;; 1. Bindings (name = val;)
    [(struct binding (name val))
     (format "~a = ~a;" name (to-nix val level))]

    ;; 2. Sets ({ ... })
    [(struct nix-set (bindings))
     (if (empty? bindings)
         "{ }"
         (let ([child-lvl (add1 level)])
           (string-append
            "{\n" 
            (string-join 
             (map (lambda (b) 
                    (string-append (indent child-lvl) (to-nix b child-lvl)))
                  bindings)
             "\n")
            "\n" (indent level) "}")))]

    ;; 3. Lists ([ ... ])
    [(struct nix-list (elems))
     (if (empty? elems)
         "[ ]"
         (format "[ ~a ]" (string-join (map (lambda (e) (to-nix e level)) elems) " ")))]

    ;; 4. Lambdas ({ args }: body)
    [(struct nix-lambda (args body))
     (format "{ ~a, ... }:\n~a~a"
             (string-join args ", ")
             (indent level)
             (to-nix body level))]

    ;; 5. Function Calls (func arg)
    [(struct nix-call (func arg))
     (format "~a ~a" func (to-nix arg level))]

    ;; 6. Comments (# text \n node)
    [(struct nix-comment (text node))
     (format "# ~a\n~a~a" 
             text 
             (indent level) 
             (to-nix node level))]

    ;; 7. Primitives
    [(struct nix-var (name)) name]
    [(? string? s) (format "\"~a\"" s)]
    [(? boolean? b) (if b "true" "false")]
    [(? number? n) (number->string n)]

    ;; 8. Let bindings
    [(struct nix-let (bindings body))
     (format "let\n~a\nin\n~a~a"
             ;; Reuse nix-set logic to print bindings, but without braces
             (string-join 
              (map (lambda (b) 
                     (string-append (indent (add1 level)) (to-nix b (add1 level))))
                   bindings)
              "\n")
             (indent level)
             (to-nix body level))]
    
    [else (error "Unknown AST node:" expr)]))
