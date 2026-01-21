#lang racket

(require "structs.rkt")
(provide to-nix to-nix-mapped)

(define (indent n) (make-string (* n 2) #\space))

;; Helper to check if expression needs parentheses
(define (needs-parens? expr)
  (or (nix-binop? expr)
      (nix-if? expr)
      (nix-lambda? expr)
      (nix-with? expr)
      (nix-assert? expr)))

(define (maybe-wrap expr str)
  (if (needs-parens? expr)
      (format "(~a)" str)
      str))

;; Convert attribute path to string
(define (attrpath->string path)
  (string-join
   (map (lambda (p)
          (cond
            [(string? p)
             (if (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_'-]*$" p)
                 p
                 (format "\"~a\"" p))]
            [(nix-string? p) (to-nix p)]
            [else (to-nix p)]))
        path)
   "."))

;; =============================================================================
;; SIMPLE CODE GENERATION (no source mapping)
;; =============================================================================

(define (to-nix expr [level 0])
  (match expr
    ;; --- Literals ---
    [(struct nix-var (name)) name]
    [(struct nix-int (v)) (number->string v)]
    [(struct nix-float (v)) (number->string v)]
    [(struct nix-bool (v)) (if v "true" "false")]
    [(struct nix-null ()) "null"]
    [(struct nix-path (v))
     (if (string? v)
         v
         (to-nix v level))]  ; For interpolated paths, compile the nix-string
    [(struct nix-uri (v)) v]

    ;; --- Strings ---
    [(struct nix-string (parts))
     (if (and (= (length parts) 1) (string? (car parts)))
         (format "\"~a\"" (car parts))
         (string-append "\""
                        (string-join
                         (map (lambda (p)
                                (if (string? p)
                                    p
                                    (format "${~a}" (to-nix p level))))
                              parts)
                         "")
                        "\""))]

    [(struct nix-interp (e)) (to-nix e level)]

    ;; --- Legacy: raw strings, booleans, numbers ---
    [(? string? s) (format "\"~a\"" s)]
    [(? boolean? b) (if b "true" "false")]
    [(? number? n) (number->string n)]

    ;; --- Bindings ---
    [(struct binding (name val))
     (define name-str (if (list? name) (attrpath->string name) name))
     (if (or (nix-let? val) (nix-if? val))
         (format "~a =\n~a~a;" name-str (indent (add1 level)) (to-nix val (add1 level)))
         (format "~a = ~a;" name-str (to-nix val level)))]

    [(struct inherit-binding (names))
     (format "inherit ~a;" (string-join names " "))]

    [(struct inherit-from-binding (from names))
     (format "inherit (~a) ~a;" (to-nix from level) (string-join names " "))]

    ;; --- Sets ---
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

    [(struct nix-rec-set (bindings))
     (if (empty? bindings)
         "rec { }"
         (let ([child-lvl (add1 level)])
           (string-append
            "rec {\n"
            (string-join
             (map (lambda (b)
                    (string-append (indent child-lvl) (to-nix b child-lvl)))
                  bindings)
             "\n")
            "\n" (indent level) "}")))]

    ;; --- Lists ---
    [(struct nix-list (elems))
     (if (empty? elems)
         "[ ]"
         (format "[ ~a ]" (string-join (map (lambda (e) (to-nix e level)) elems) " ")))]

    ;; --- Lambdas ---
    [(struct nix-lambda (param body))
     (define param-str
       (match param
         [(struct param-id (name)) (format "~a:" name)]
         [(struct param-set (args variadic? at-name))
          (define args-str
            (string-join
             (map (lambda (a)
                    (match a
                      [(struct param-arg (name default))
                       (if default
                           (format "~a ? ~a" name (to-nix default level))
                           name)]))
                  args)
             ", "))
          (define full-args (if variadic?
                                (if (string=? args-str "") "..." (string-append args-str ", ..."))
                                args-str))
          (define set-part (format "{ ~a }" full-args))
          (if at-name
              (format "~a@~a:" set-part at-name)
              (format "~a:" set-part))]
         ;; Legacy: list of argument names
         [(? list? args)
          (format "{ ~a }:" (string-join args ", "))]))
     (format "~a\n~a~a" param-str (indent level) (to-nix body level))]

    ;; --- Function Calls ---
    [(struct nix-call (func arg))
     (define func-str (to-nix func level))
     (define arg-str (to-nix arg level))
     (format "~a ~a" func-str (maybe-wrap arg arg-str))]

    ;; --- Control Flow ---
    [(struct nix-if (cond then-e else-e))
     (format "if ~a then ~a else ~a"
             (to-nix cond level)
             (to-nix then-e level)
             (to-nix else-e level))]

    [(struct nix-let (bindings body))
     (if (empty? bindings)
         (to-nix body level)
         (format "let\n~a\n~ain\n~a~a"
                 (string-join
                  (map (lambda (b)
                         (string-append (indent (add1 level)) (to-nix b (add1 level))))
                       bindings)
                  "\n")
                 (indent level)
                 (indent level)
                 (to-nix body level)))]

    [(struct nix-with (env body))
     (format "with ~a;\n~a~a" (to-nix env level) (indent level) (to-nix body level))]

    [(struct nix-assert (cond body))
     (format "assert ~a;\n~a~a" (to-nix cond level) (indent level) (to-nix body level))]

    ;; --- Operators ---
    [(struct nix-binop (op left right))
     (format "~a ~a ~a" (to-nix left level) op (to-nix right level))]

    [(struct nix-unop (op arg))
     (format "~a~a" op (maybe-wrap arg (to-nix arg level)))]

    [(struct nix-select (expr path default))
     (define base (format "~a.~a"
                          (maybe-wrap expr (to-nix expr level))
                          (attrpath->string path)))
     (if default
         (format "~a or ~a" base (to-nix default level))
         base)]

    [(struct nix-has-attr (expr path))
     (format "~a ? ~a"
             (maybe-wrap expr (to-nix expr level))
             (attrpath->string path))]

    ;; --- Comments ---
    [(struct nix-comment (text node))
     (format "# ~a\n~a~a" text (indent level) (to-nix node level))]

    [else (error "Unknown AST node:" expr)]))

;; =============================================================================
;; CODE GENERATION WITH SOURCE MAPPING
;; =============================================================================

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
    ;; --- Literals ---
    [(struct nix-var (name))
     (values name (list (cons path (cons 0 (string-length name)))))]
    [(struct nix-int (v))
     (define s (number->string v))
     (values s (list (cons path (cons 0 (string-length s)))))]
    [(struct nix-float (v))
     (define s (number->string v))
     (values s (list (cons path (cons 0 (string-length s)))))]
    [(struct nix-bool (v))
     (define s (if v "true" "false"))
     (values s (list (cons path (cons 0 (string-length s)))))]
    [(struct nix-null ())
     (values "null" (list (cons path (cons 0 4))))]
    [(struct nix-path (v))
     (if (string? v)
         (values v (list (cons path (cons 0 (string-length v)))))
         ;; Interpolated path
         (let-values ([(s m) (to-nix-mapped v level path)])
           (values s m)))]
    [(struct nix-uri (v))
     (values v (list (cons path (cons 0 (string-length v)))))]

    ;; --- Strings ---
    [(struct nix-string (parts))
     (define s (to-nix expr level))
     (values s (list (cons path (cons 0 (string-length s)))))]

    [(struct nix-interp (e))
     (to-nix-mapped e level path)]

    ;; --- Legacy primitives ---
    [(? string? s)
     (let ([res (format "\"~a\"" s)])
       (values res (list (cons path (cons 0 (string-length res))))))]
    [(? boolean? b)
     (let ([res (if b "true" "false")])
       (values res (list (cons path (cons 0 (string-length res))))))]
    [(? number? n)
     (let ([res (number->string n)])
       (values res (list (cons path (cons 0 (string-length res))))))]

    ;; --- Bindings ---
    [(struct binding (name val))
     (define name-str (if (list? name) (attrpath->string name) name))
     (let-values ([(v-str v-map) (to-nix-mapped val level path)])
       (let* ([s1 (format "~a = " name-str)]
              [full-str (string-append s1 v-str ";")])
         (values full-str (shift-map v-map (string-length s1)))))]

    [(struct inherit-binding (names))
     (define s (format "inherit ~a;" (string-join names " ")))
     (values s (list (cons path (cons 0 (string-length s)))))]

    [(struct inherit-from-binding (from names))
     (let-values ([(from-str from-map) (to-nix-mapped from level path)])
       (define s (format "inherit (~a) ~a;" from-str (string-join names " ")))
       (values s (shift-map from-map 9)))] ; "inherit (" = 9 chars

    ;; --- Sets ---
    [(struct nix-set (bindings))
     (if (empty? bindings)
         (values "{ }" (list (cons path (cons 0 3))))
         (let ([child-lvl (add1 level)])
           (let-values ([(body-str body-map)
                         (for/fold ([acc-str ""] [acc-map '()])
                                   ([b bindings])
                           (let* ([key (cond
                                         [(binding? b)
                                          (define n (binding-name b))
                                          (if (list? n) (attrpath->string n) n)]
                                         [else #f])]
                                  [child-path (if key (append path (list key)) path)]
                                  [indent-str (indent child-lvl)])
                             (let-values ([(b-str b-map)
                                           (if (binding? b)
                                               (to-nix-mapped (binding-value b) child-lvl child-path)
                                               (to-nix-mapped b child-lvl path))])
                               (let* ([line (if (binding? b)
                                                (let ([name-str (if (list? (binding-name b))
                                                                    (attrpath->string (binding-name b))
                                                                    (binding-name b))])
                                                  (let ([sep (if (is-let? (binding-value b))
                                                                 (format " =\n~a" (indent (add1 child-lvl)))
                                                                 " = ")])
                                                    (format "~a~a~a~a;\n" indent-str name-str sep b-str)))
                                                (format "~a~a\n" indent-str b-str))]
                                      [prefix-len (+ (string-length indent-str)
                                                     (if (binding? b)
                                                         (+ (string-length (if (list? (binding-name b))
                                                                               (attrpath->string (binding-name b))
                                                                               (binding-name b)))
                                                            (if (is-let? (binding-value b))
                                                                (+ 3 (string-length (indent (add1 child-lvl))))
                                                                3))
                                                         0))]
                                      [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
                                 (values (string-append acc-str line)
                                         (append acc-map shifted-b-map))))))])
             (let* ([start-str "{\n"]
                    [end-str (format "~a}" (indent level))]
                    [full-str (string-append start-str body-str end-str)]
                    [total-len (string-length full-str)]
                    [shifted-body-map (shift-map body-map (string-length start-str))])
               (values full-str (cons (cons path (cons 0 total-len)) shifted-body-map))))))]

    [(struct nix-rec-set (bindings))
     ;; Similar to nix-set but with "rec " prefix
     (define inner-set (nix-set bindings))
     (let-values ([(s m) (to-nix-mapped inner-set level path)])
       (values (string-append "rec " s) (shift-map m 4)))]

    ;; --- Lists ---
    [(struct nix-list (elems))
     (if (empty? elems)
         (values "[ ]" (list (cons path (cons 0 3))))
         (let-values ([(body-str body-map)
                       (for/fold ([acc-str ""] [acc-map '()]
                                  #:result (values (if (string=? acc-str "") ""
                                                       (substring acc-str 0 (sub1 (string-length acc-str))))
                                                   acc-map))
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

    ;; --- Lambdas ---
    [(struct nix-lambda (param body))
     (let-values ([(body-str body-map) (to-nix-mapped body level (append path '("body")))])
       (define param-str
         (match param
           [(struct param-id (name)) (format "~a:" name)]
           [(struct param-set (args variadic? at-name))
            (define args-str
              (string-join
               (map (lambda (a)
                      (match a
                        [(struct param-arg (name default))
                         (if default
                             (format "~a ? ~a" name (to-nix default level))
                             name)]))
                    args)
               ", "))
            (define full-args (if variadic?
                                  (if (string=? args-str "") "..." (string-append args-str ", ..."))
                                  args-str))
            (define set-part (format "{ ~a }" full-args))
            (if at-name
                (format "~a@~a:" set-part at-name)
                (format "~a:" set-part))]
           [(? list? args)
            (format "{ ~a }:" (string-join args ", "))]))
       (let* ([header (format "~a\n~a" param-str (indent level))]
              [full-str (string-append header body-str)]
              [shifted-map (shift-map body-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map))))]

    ;; --- Function Calls ---
    [(struct nix-call (func arg))
     (let-values ([(func-str func-map) (to-nix-mapped func level path)]
                  [(arg-str arg-map) (to-nix-mapped arg level path)])
       (let* ([wrapped-arg (maybe-wrap arg arg-str)]
              [sep " "]
              [full-str (string-append func-str sep wrapped-arg)]
              [shifted-arg-map (shift-map arg-map (+ (string-length func-str) (string-length sep)))])
         (values full-str (cons (cons path (cons 0 (string-length full-str)))
                                (append func-map shifted-arg-map)))))]

    ;; --- Control Flow ---
    [(struct nix-if (cond-e then-e else-e))
     (let-values ([(c-str c-map) (to-nix-mapped cond-e level path)]
                  [(t-str t-map) (to-nix-mapped then-e level path)]
                  [(e-str e-map) (to-nix-mapped else-e level path)])
       (let* ([s (format "if ~a then ~a else ~a" c-str t-str e-str)]
              [if-offset 3]
              [then-offset (+ if-offset (string-length c-str) 6)]
              [else-offset (+ then-offset (string-length t-str) 6)])
         (values s (cons (cons path (cons 0 (string-length s)))
                         (append (shift-map c-map if-offset)
                                 (shift-map t-map then-offset)
                                 (shift-map e-map else-offset))))))]

    [(struct nix-let (bindings body))
     (if (empty? bindings)
         (to-nix-mapped body level path)
         (let-values ([(body-str body-map) (to-nix-mapped body level (append path '("body")))])
           (let-values ([(bind-str bind-map)
                         (for/fold ([acc-str ""] [acc-map '()]
                                    #:result (values (if (string=? acc-str "") ""
                                                         (substring acc-str 0 (sub1 (string-length acc-str))))
                                                     acc-map))
                                   ([b bindings])
                           (let* ([key (if (binding? b)
                                           (let ([n (binding-name b)])
                                             (if (list? n) (attrpath->string n) n))
                                           #f)]
                                  [child-path (if key (append path (list "bindings" key)) path)]
                                  [indent-str (indent (add1 level))])
                             (let-values ([(b-str b-map)
                                           (if (binding? b)
                                               (to-nix-mapped (binding-value b) (add1 level) child-path)
                                               (to-nix-mapped b (add1 level) path))])
                               (let* ([line (if (binding? b)
                                                (let ([sep (if (is-let? (binding-value b))
                                                               (format " =\n~a" (indent (+ 2 level)))
                                                               " = ")])
                                                  (format "~a~a~a~a;\n" indent-str key sep b-str))
                                                (format "~a~a\n" indent-str b-str))]
                                      [prefix-len (+ (string-length indent-str)
                                                     (if key (+ (string-length key) 3) 0))]
                                      [shifted-b-map (shift-map b-map (+ (string-length acc-str) prefix-len))])
                                 (values (string-append acc-str line)
                                         (append acc-map shifted-b-map))))))])
             (let* ([header "let\n"]
                    [sep (format "\n~ain\n~a" (indent level) (indent level))]
                    [full-str (string-append header bind-str sep body-str)]
                    [total-len (string-length full-str)]
                    [shifted-bind-map (shift-map bind-map (string-length header))]
                    [shifted-body-map (shift-map body-map (+ (string-length header)
                                                             (string-length bind-str)
                                                             (string-length sep)))])
               (values full-str
                       (cons (cons path (cons 0 total-len))
                             (append shifted-bind-map shifted-body-map)))))))]

    [(struct nix-with (env body))
     (let-values ([(env-str env-map) (to-nix-mapped env level path)]
                  [(body-str body-map) (to-nix-mapped body level path)])
       (let* ([header (format "with ~a;\n~a" env-str (indent level))]
              [full-str (string-append header body-str)]
              [shifted-body-map (shift-map body-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str)))
                                (append (shift-map env-map 5) shifted-body-map)))))]

    [(struct nix-assert (cond-e body))
     (let-values ([(c-str c-map) (to-nix-mapped cond-e level path)]
                  [(body-str body-map) (to-nix-mapped body level path)])
       (let* ([header (format "assert ~a;\n~a" c-str (indent level))]
              [full-str (string-append header body-str)]
              [shifted-body-map (shift-map body-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str)))
                                (append (shift-map c-map 7) shifted-body-map)))))]

    ;; --- Operators ---
    [(struct nix-binop (op left right))
     (let-values ([(l-str l-map) (to-nix-mapped left level path)]
                  [(r-str r-map) (to-nix-mapped right level path)])
       (let* ([full-str (format "~a ~a ~a" l-str op r-str)]
              [right-offset (+ (string-length l-str) (string-length op) 2)])
         (values full-str (cons (cons path (cons 0 (string-length full-str)))
                                (append l-map (shift-map r-map right-offset))))))]

    [(struct nix-unop (op arg))
     (let-values ([(a-str a-map) (to-nix-mapped arg level path)])
       (let* ([wrapped (maybe-wrap arg a-str)]
              [full-str (format "~a~a" op wrapped)])
         (values full-str (cons (cons path (cons 0 (string-length full-str)))
                                (shift-map a-map (string-length op))))))]

    [(struct nix-select (expr attr-path default))
     (let-values ([(e-str e-map) (to-nix-mapped expr level path)])
       (let* ([wrapped-e (maybe-wrap expr e-str)]
              [path-str (attrpath->string attr-path)]
              [base (format "~a.~a" wrapped-e path-str)]
              [full-str (if default
                            (format "~a or ~a" base (to-nix default level))
                            base)])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) e-map))))]

    [(struct nix-has-attr (expr attr-path))
     (let-values ([(e-str e-map) (to-nix-mapped expr level path)])
       (let* ([wrapped-e (maybe-wrap expr e-str)]
              [path-str (attrpath->string attr-path)]
              [full-str (format "~a ? ~a" wrapped-e path-str)])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) e-map))))]

    ;; --- Comments ---
    [(struct nix-comment (text node))
     (let-values ([(n-str n-map) (to-nix-mapped node level path)])
       (let* ([header (format "# ~a\n~a" text (indent level))]
              [full-str (string-append header n-str)]
              [shifted-map (shift-map n-map (string-length header))])
         (values full-str (cons (cons path (cons 0 (string-length full-str))) shifted-map))))]

    [else (values (format "<unknown:~a>" expr) '())]))
