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

  (define (is-let? e)
    (or (nix-let? e) (and (nix-comment? e) (is-let? (nix-comment-node e)))))

  (match expr
    ;; 1. Bindings
    [(struct binding (name val))
     (let-values ([(v-str v-map) (to-nix-mapped val level path)])
       (let* ([s1 (format "~a = " name)]
              [full-str (string-append s1 v-str ";")]
              [shifted-v-map (shift-map v-map (string-length s1))])
         (values full-str shifted-v-map)))]

    ;; 2. Sets
    [(struct nix-set (bindings))
     (if (empty? bindings)
         (values "{ }" (list (cons path (cons 0 3))))
         (let ([child-lvl (add1 level)])
           (let-values ([(body-str body-map)
                         (for/fold ([acc-str ""] [acc-map '()])
                                   ([b bindings])
                           (let* ([key (binding-name b)]
                                  [val (binding-value b)]
                                  [child-path (append path (list key))]
                                  [indent-str (indent child-lvl)])
                             (let-values ([(b-str b-map) (to-nix-mapped val child-lvl child-path)])
                               (let* ([sep (if (is-let? val) (format " =\n~a" (indent child-lvl)) " = ")]
                                      [line (format "~a~a~a~a;\n" indent-str key sep b-str)]
                                      [prefix-len (+ (string-length indent-str) (string-length key) (string-length sep))]
                                      [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
                                 (values (string-append acc-str line)
                                         (append acc-map shifted-b-map))))))])
             (let* ([start-str "{\n"]
                    [end-str (format "~a}" (indent level))]
                    [full-str (string-append start-str body-str end-str)]
                    [total-len (string-length full-str)]
                    [shifted-body-map (shift-map body-map (string-length start-str))])
               (values full-str (cons (cons path (cons 0 total-len)) shifted-body-map))))))]

    ;; 3. Lists
    [(struct nix-list (elems))
     (if (empty? elems)
         (values "[ ]" (list (cons path (cons 0 3))))
         (let-values ([(body-str body-map)
                       (for/fold ([acc-str ""] [acc-map '()] #:result (values (if (string=? acc-str "") "" (substring acc-str 0 (sub1 (string-length acc-str)))) acc-map))
                                 ([e elems] [i (in-naturals)])
                         (let* ([child-path (append path (list (number->string i)))]
                                [child-vals (call-with-values (lambda () (to-nix-mapped e level child-path)) list)]
                                [e-str (first child-vals)]
                                [e-map (second child-vals)]
                                [shifted-e-map (shift-map e-map (string-length acc-str))])
                           (values (string-append acc-str e-str " ")
                                   (append acc-map shifted-e-map))))])
           (let* ([full-str (format "[ ~a ]" body-str)]
                  [shifted-body-map (shift-map body-map 2)]
                  [total-len (string-length full-str)])
             (values full-str (cons (cons path (cons 0 total-len)) shifted-body-map)))))]

    ;; 4. Lambdas
    [(struct nix-lambda (args body))
     (let-values ([(body-str body-map) (to-nix-mapped body level path)])
       (let* ([header (format "{ ~a, ... }:\n~a" (string-join args ", ") (indent level))]
              [full-str (string-append header body-str)]
              [shifted-map (shift-map body-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map))))]

    ;; 5. Function Calls
    [(struct nix-call (func arg))
     (let-values ([(arg-str arg-map) (to-nix-mapped arg level path)])
       (let* ([header (format "~a " func)]
              [full-str (string-append header arg-str)]
              [shifted-map (shift-map arg-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map))))]

    ;; 6. Comments
    [(struct nix-comment (text node))
     (let-values ([(n-str n-map) (to-nix-mapped node level path)])
       (let* ([header (format "# ~a\n~a" text (indent level))]
              [full-str (string-append header n-str)]
              [shifted-map (shift-map n-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map))))]

    ;; 7. Primitives
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

    ;; 8. Let bindings
    [(struct nix-let (bindings body))
     (let-values ([(body-str body-map) (to-nix-mapped body level (append path '("body")))])
       (let-values ([(bind-str bind-map)
                     (for/fold ([acc-str ""] [acc-map '()] #:result (values (if (string=? acc-str "") "" (substring acc-str 0 (sub1 (string-length acc-str)))) acc-map))
                               ([b bindings])
                       (let* ([key (binding-name b)]
                              [val (binding-value b)]
                              [child-path (append path (list "bindings" key))]
                              [indent-str (indent (add1 level))])
                         (let-values ([(b-str b-map) (to-nix-mapped val (add1 level) child-path)])
                           (let* ([sep (if (is-let? val) (format " =\n~a" (indent (add1 level))) " = ")]
                                  [line (format "~a~a~a~a;\n" indent-str key sep b-str)]
                                  [prefix-len (+ (string-length indent-str) (string-length key) (string-length sep))]
                                  [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
                             (values (string-append acc-str line)
                                     (append acc-map shifted-b-map))))))])
         (let* ([header "let\n"]
                [sep (format "\n~ain\n~a" (indent level) (indent level))]
                [full-str (string-append header bind-str sep body-str)]
                [total-len (string-length full-str)]
                [shifted-bind-map (shift-map bind-map (string-length header))]
                [shifted-body-map (shift-map body-map (+ (string-length header) (string-length bind-str) (string-length sep)))])
           (values full-str 
                   (cons (cons path (cons 0 total-len))
                         (append shifted-bind-map shifted-body-map))))))]

    [else (values (format "<unknown:~a>" expr) '())]))

(define (to-nix expr [level 0])
  (match expr
    ;; 1. Bindings
    [(struct binding (name val))
     (if (nix-let? val)
         (format "~a =\n~a~a;" name (indent level) (to-nix val level))
         (format "~a = ~a;" name (to-nix val level)))]

    ;; 2. Sets
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

    ;; 3. Lists
    [(struct nix-list (elems))
     (if (empty? elems)
         "[ ]"
         (format "[ ~a ]" (string-join (map (lambda (e) (to-nix e level)) elems) " ")))]

    ;; 4. Lambdas
    [(struct nix-lambda (args body))
     (format "{ ~a, ... }: \n~a~a"
             (string-join args ", ")
             (indent level)
             (to-nix body level))]

    ;; 5. Function Calls
    [(struct nix-call (func arg))
     (format "~a ~a" func (to-nix arg level))]

    ;; 6. Comments
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
     (format "let\n~a\n~ain\n~a~a"
             (string-join 
              (map (lambda (b) 
                     (string-append (indent (add1 level)) (to-nix b (add1 level))))
                   bindings)
              "\n")
             (indent level)
             (indent level)
             (to-nix body level))]
    
    [else (error "Unknown AST node:" expr)]))