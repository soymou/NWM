#lang racket

(require "structs.rkt")
(provide to-nix to-nix-mapped)

(define (indent n) (make-string (* n 2) #\space))

;; Returns (values string mappings)
;; mappings: list of (cons path (cons start end))
(define (to-nix-mapped expr [level 0] [path '()])
  (define (shift-map m offset)
    (map (lambda (entry)
           (match-let ([(cons p (cons s e)) entry])
             (cons p (cons (+ s offset) (+ e offset)))))
         m))

  (match expr
    ;; 1. Bindings (name = val;)
    [(struct binding (name val))
     (define-values (v-str v-map) (to-nix-mapped val level path)) ;; path is same as parent? No, binding doesn't add to path usually, but the SET does.
     ;; Wait, usually we traverse INTO the binding value.
     ;; The binding itself is not a "navigable" node in the path sense of `manager.rkt` unless we are IN a set.
     ;; The path passed here should be the path TO the value if we consider "name" part of the path?
     ;; In `manager.rkt`, path is list of keys.
     ;; So if we are at root, and have { a = 1; }, path to 1 is '("a").
     ;; The `binding` struct itself is just a helper.
     
     ;; Let's assume the caller passes the path TO the value for the value part.
     (let* ([s1 (format "~a = " name)]
            [s2 v-str]
            [s3 ";"]
            [full-str (string-append s1 s2 s3)]
            [shifted-v-map (shift-map v-map (string-length s1))])
       (values full-str shifted-v-map))]

    ;; 2. Sets ({ ... })
    [(struct nix-set (bindings))
     (if (empty? bindings)
         (values "{ }" (list (cons path (cons 0 3))))
         (let ([child-lvl (add1 level)])
           (define-values (body-str body-map)
             (for/fold ([acc-str ""] [acc-map '()])
                       ([b bindings])
               (let* ([key (binding-name b)]
                      [child-path (append path (list key))]
                      [indent-str (indent child-lvl)])
                 ;; Recurse
                 (define-values (b-str b-map) (to-nix-mapped (binding-value b) child-lvl child-path))
                 (let* ([line (format "~a~a = ~a;\n" indent-str key b-str)]
                        ;; We need to map the value part inside this line
                        ;; "  key = " is prefix. length = indent + key + " = "
                        [prefix-len (+ (string-length indent-str) (string-length key) 3)]
                        [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
                   (values (string-append acc-str line)
                           (append acc-map shifted-b-map))))))
           
           (let* ([start-str "{\n"]
                  [end-str (format "~a}" (indent level))]
                  [full-str (string-append start-str body-str end-str)]
                  [total-len (string-length full-str)]
                  [shifted-body-map (shift-map body-map (string-length start-str))])
             (values full-str (cons (cons path (cons 0 total-len)) shifted-body-map)))))]

    ;; 3. Lists ([ ... ])
    [(struct nix-list (elems))
     (if (empty? elems)
         (values "[ ]" (list (cons path (cons 0 3))))
         (let-values ([(body-str body-map)
                       (for/fold ([acc-str ""] [acc-map '()] #:result (values (substring acc-str 0 (sub1 (string-length acc-str))) acc-map)) ;; remove trailing space
                                 ([e elems] [i (in-naturals)])
                         (let* ([child-path (append path (list (number->string i)))]
                                [child-vals (call-with-values (lambda () (to-nix-mapped e level child-path)) list)]
                                [e-str (first child-vals)]
                                [e-map (second child-vals)]
                                [shifted-e-map (shift-map e-map (string-length acc-str))])
                           (values (string-append acc-str e-str " ")
                                   (append acc-map shifted-e-map))))])
           (let* ([full-str (format "[ ~a ]" body-str)]
                  [shifted-body-map (shift-map body-map 2)] ;; "[ " is 2 chars
                  [total-len (string-length full-str)])
             (values full-str (cons (cons path (cons 0 total-len)) shifted-body-map)))))]

    ;; 4. Lambdas ({ args }: body)
    [(struct nix-lambda (args body))
     (define-values (body-str body-map) (to-nix-mapped body level path)) ;; Path usually stays same for lambda body? Or should we have a virtual path?
     ;; Manager traversal doesn't seem to support traversing into lambda args/body explicitly via keys yet?
     ;; Manager says: `update-in` only supports sets, lists, lets.
     ;; So we treat lambda as a leaf or opaque for now in terms of path?
     ;; If the path doesn't change, we just return the body map.
     (let* ([header (format "{ ~a, ... }:\n~a" (string-join args ", ") (indent level))]
            [full-str (string-append header body-str)]
            [shifted-map (shift-map body-map (string-length header))])
       (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map)))]

    ;; 5. Primitives & Others
    [(struct nix-var (name)) 
     (values name (list (cons path (cons 0 (string-length name)))))]
    
    [(? string? s) 
     (let ([res (format "\"~a\"" s)])
       (values res (list (cons path (cons 0 (string-length res))))))]
    
    [(? boolean? b) 
     (let ([res (if b "true" "false")])
       (values res (list (cons path (cons 0 (string-length res))))))]
    
    [(? number? n) 
     (let ([res (number->string n)])
       (values res (list (cons path (cons 0 (string-length res))))))]

    ;; 8. Let bindings (Complex)
    [(struct nix-let (bindings body))
     (define-values (body-str body-map) (to-nix-mapped body level (append path '("body"))))
     
     (define-values (bind-str bind-map)
       (for/fold ([acc-str ""] [acc-map '()])
                 ([b bindings])
         (let* ([key (binding-name b)]
                [child-path (append path (list "bindings" key))]
                [indent-str (indent (add1 level))])
           (define-values (b-str b-map) (to-nix-mapped (binding-value b) (add1 level) child-path))
           (let* ([line (format "~a~a = ~a;\n" indent-str key b-str)]
                  [prefix-len (+ (string-length indent-str) (string-length key) 3)]
                  [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
             (values (string-append acc-str line)
                     (append acc-map shifted-b-map))))))
     
     (let* ([header "let\n"]
            [sep (format "\nin\n~a" (indent level))]
            [full-str (string-append header bind-str sep body-str)]
            [total-len (string-length full-str)]
            [shifted-bind-map (shift-map bind-map (string-length header))]
            [shifted-body-map (shift-map body-map (+ (string-length header) (string-length bind-str) (string-length sep)))])
       (values full-str 
               (cons (cons path (cons 0 total-len))
                     (append shifted-bind-map shifted-body-map))))]

    [else (values (format "<unknown:~a>" expr) '())]))

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
