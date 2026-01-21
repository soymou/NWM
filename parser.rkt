#lang racket

(require "structs.rkt")

(provide parse-nix-structure-strict parse-nix-structure)

;; =============================================================================
;; TOKENIZER
;; =============================================================================

;; Token structure
(struct token (type value pos) #:transparent)

;; Keywords
(define keywords '("if" "then" "else" "let" "in" "with" "assert" "inherit" "rec" "or"))

;; Multi-char operators (order matters - longer first)
(define multi-ops '("->" "==" "!=" "<=" ">=" "&&" "||" "++" "//" "..."))

;; Single-char operators and delimiters
(define single-ops '(#\{ #\} #\[ #\] #\( #\) #\= #\; #\: #\, #\. #\@ #\? #\! #\+ #\- #\* #\/ #\< #\>))

(define (tokenize input)
  (define len (string-length input))
  (define pos 0)
  (define tokens '())

  (define (peek [offset 0])
    (if (< (+ pos offset) len)
        (string-ref input (+ pos offset))
        #f))

  (define (advance! [n 1])
    (set! pos (+ pos n)))

  (define (peek-string n)
    (if (<= (+ pos n) len)
        (substring input pos (+ pos n))
        #f))

  (define (emit! type value)
    (set! tokens (cons (token type value pos) tokens)))

  (define (skip-whitespace)
    (let loop ()
      (when (and (peek) (char-whitespace? (peek)))
        (advance!)
        (loop))))

  (define (skip-line-comment)
    ;; Skip # until newline
    (advance!) ; skip #
    (let loop ()
      (when (and (peek) (not (char=? (peek) #\newline)))
        (advance!)
        (loop))))

  (define (skip-block-comment)
    ;; Skip /* until */
    (advance! 2) ; skip /*
    (let loop ([depth 1])
      (cond
        [(not (peek)) (error "Unterminated block comment")]
        [(and (char=? (peek) #\*) (equal? (peek 1) #\/))
         (advance! 2)
         (when (> depth 1) (loop (sub1 depth)))]
        [(and (char=? (peek) #\/) (equal? (peek 1) #\*))
         (advance! 2)
         (loop (add1 depth))]
        [else
         (advance!)
         (loop depth)])))

  (define (read-string)
    ;; Read "..." string with interpolation support
    (define start pos)
    (advance!) ; skip opening "
    (define parts '())
    (define current-str "")

    (let loop ()
      (cond
        [(not (peek)) (error "Unterminated string")]
        [(char=? (peek) #\")
         (advance!)
         (unless (string=? current-str "")
           (set! parts (cons current-str parts)))
         (if (and (= (length parts) 1) (string? (car parts)))
             (emit! 'STRING (car parts))
             (emit! 'INTERP-STRING (reverse parts)))]
        [(char=? (peek) #\\)
         ;; Escape sequence
         (advance!)
         (when (peek)
           (set! current-str (string-append current-str (string (peek))))
           (advance!))
         (loop)]
        [(and (char=? (peek) #\$) (equal? (peek 1) #\{))
         ;; Interpolation
         (unless (string=? current-str "")
           (set! parts (cons current-str parts)))
         (set! current-str "")
         (advance! 2) ; skip ${
         ;; Tokenize the interpolated expression
         (define interp-tokens (tokenize-until-brace))
         (set! parts (cons (cons 'INTERP interp-tokens) parts))
         (loop)]
        [else
         (set! current-str (string-append current-str (string (peek))))
         (advance!)
         (loop)])))

  (define (tokenize-until-brace)
    ;; Tokenize until matching }
    (define saved-tokens tokens)
    (define saved-pos pos)
    (set! tokens '())
    (let loop ([depth 1])
      (skip-whitespace-and-comments)
      (cond
        [(not (peek)) (error "Unterminated interpolation")]
        [(char=? (peek) #\})
         (if (= depth 1)
             (let ([result (reverse tokens)])
               (advance!) ; skip }
               (set! tokens saved-tokens)
               result)
             (begin
               (emit! 'RBRACE "}")
               (advance!)
               (loop (sub1 depth))))]
        [(char=? (peek) #\{)
         (emit! 'LBRACE "{")
         (advance!)
         (loop (add1 depth))]
        [else
         (read-token)
         (loop depth)])))

  (define (read-multiline-string)
    ;; Read ''...'' string
    (advance! 2) ; skip ''
    (define parts '())
    (define current-str "")

    (let loop ()
      (cond
        [(not (peek)) (error "Unterminated multiline string")]
        [(and (char=? (peek) #\') (equal? (peek 1) #\'))
         (cond
           ;; Check for escape sequences
           [(equal? (peek 2) #\')
            ;; ''' = escaped '
            (set! current-str (string-append current-str "'"))
            (advance! 3)
            (loop)]
           [(equal? (peek 2) #\$)
            ;; ''$ = escaped $
            (set! current-str (string-append current-str "$"))
            (advance! 3)
            (loop)]
           [(equal? (peek 2) #\\)
            ;; ''\ = escape sequence
            (advance! 3)
            (when (peek)
              (set! current-str (string-append current-str (string (peek))))
              (advance!))
            (loop)]
           [else
            ;; End of string
            (advance! 2)
            (unless (string=? current-str "")
              (set! parts (cons current-str parts)))
            (if (and (= (length parts) 1) (string? (car parts)))
                (emit! 'STRING (car parts))
                (emit! 'INTERP-STRING (reverse parts)))])]
        [(and (char=? (peek) #\$) (equal? (peek 1) #\{))
         ;; Interpolation
         (unless (string=? current-str "")
           (set! parts (cons current-str parts)))
         (set! current-str "")
         (advance! 2)
         (define interp-tokens (tokenize-until-brace))
         (set! parts (cons (cons 'INTERP interp-tokens) parts))
         (loop)]
        [else
         (set! current-str (string-append current-str (string (peek))))
         (advance!)
         (loop)])))

  (define (read-path-or-uri)
    ;; Read path literal or URI, with interpolation support
    (define start pos)
    (define parts '())
    (define current-segment "")

    (define (path-char? c)
      (and c (or (char-alphabetic? c) (char-numeric? c)
                 (member c '(#\/ #\. #\_ #\- #\+ #\~ #\:)))))

    (let loop ()
      (define c (peek))
      (cond
        ;; Interpolation
        [(and (char=? c #\$) (peek 1) (char=? (peek 1) #\{))
         (unless (string=? current-segment "")
           (set! parts (cons current-segment parts)))
         (set! current-segment "")
         (advance! 2) ; skip ${
         (define interp-tokens (tokenize-until-brace))
         (set! parts (cons (cons 'INTERP interp-tokens) parts))
         (loop)]
        ;; Continue path
        [(path-char? c)
         (set! current-segment (string-append current-segment (string c)))
         (advance!)
         (loop)]
        ;; End of path
        [else
         (unless (string=? current-segment "")
           (set! parts (cons current-segment parts)))]))

    (define all-parts (reverse parts))
    (define first-part (if (and (pair? all-parts) (string? (car all-parts)))
                           (car all-parts)
                           ""))

    ;; Determine type
    (cond
      ;; Single-part path (no interpolation)
      [(and (= (length all-parts) 1) (string? (car all-parts)))
       (define val (car all-parts))
       (cond
         [(regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9+.-]*:" val)
          (emit! 'URI val)]
         [(or (string-prefix? val "./") (string-prefix? val "/")
              (string-prefix? val "../") (string-prefix? val "~/"))
          (emit! 'PATH val)]
         [else
          ;; It's an identifier, back up
          (set! pos start)
          (read-identifier)])]
      ;; Path with interpolation
      [(or (string-prefix? first-part "./") (string-prefix? first-part "/")
           (string-prefix? first-part "../") (string-prefix? first-part "~/"))
       (emit! 'INTERP-PATH all-parts)]
      [else
       ;; Back up and try as identifier
       (set! pos start)
       (read-identifier)]))

  (define (read-search-path)
    ;; Read <...> path
    (advance!) ; skip <
    (define start pos)
    (let loop ()
      (cond
        [(not (peek)) (error "Unterminated search path")]
        [(char=? (peek) #\>)
         (define val (substring input start pos))
         (advance!)
         (emit! 'PATH (string-append "<" val ">"))]
        [else (advance!) (loop)])))

  (define (read-number)
    (define start pos)
    (define has-dot #f)
    (let loop ()
      (define c (peek))
      (cond
        [(and c (char-numeric? c)) (advance!) (loop)]
        [(and c (char=? c #\.) (not has-dot) (peek 1) (char-numeric? (peek 1)))
         (set! has-dot #t)
         (advance!)
         (loop)]
        [else
         (define val (substring input start pos))
         (if has-dot
             (emit! 'FLOAT (string->number val))
             (emit! 'INT (string->number val)))]))
    )

  (define (read-identifier)
    (define start pos)
    (let loop ()
      (define c (peek))
      (when (and c (or (char-alphabetic? c) (char-numeric? c)
                       (char=? c #\_) (char=? c #\') (char=? c #\-)))
        (advance!)
        (loop)))
    (define val (substring input start pos))
    (cond
      [(member val keywords) (emit! 'KEYWORD val)]
      [(equal? val "true") (emit! 'BOOL #t)]
      [(equal? val "false") (emit! 'BOOL #f)]
      [(equal? val "null") (emit! 'NULL #f)]
      [else (emit! 'ID val)]))

  (define (skip-whitespace-and-comments)
    (let loop ()
      (skip-whitespace)
      (cond
        [(and (peek) (char=? (peek) #\#))
         (skip-line-comment)
         (loop)]
        [(and (peek) (char=? (peek) #\/) (equal? (peek 1) #\*))
         (skip-block-comment)
         (loop)])))

  (define (read-token)
    (skip-whitespace-and-comments)
    (when (peek)
      (define c (peek))
      (define start pos)
      (cond
        ;; Multi-line string
        [(and (char=? c #\') (equal? (peek 1) #\'))
         (read-multiline-string)]
        ;; String
        [(char=? c #\")
         (read-string)]
        ;; Search path <...>
        [(char=? c #\<)
         (cond
           ;; Check if it's <= or just <
           [(equal? (peek 1) #\=) (emit! 'OP "<=") (advance! 2)]
           ;; Check if it looks like a search path
           [(and (peek 1) (char-alphabetic? (peek 1)))
            (read-search-path)]
           [else (emit! 'OP "<") (advance!)])]
        ;; Number
        [(char-numeric? c)
         (read-number)]
        ;; Path starting with ./ or ../ or / or ~/
        ;; Be careful not to match ... (ellipsis) or // (merge operator)
        [(and (char=? c #\~)
              (peek 1)
              (char=? (peek 1) #\/))
         (read-path-or-uri)]
        ;; Absolute path: /foo but NOT //
        [(and (char=? c #\/)
              (peek 1)
              (not (char=? (peek 1) #\/))  ; Not // (merge operator)
              (or (char-alphabetic? (peek 1))
                  (char-numeric? (peek 1))
                  (char=? (peek 1) #\_)))
         (read-path-or-uri)]
        [(and (char=? c #\.)
              (peek 1)
              (char=? (peek 1) #\/)  ; Must be ./ not just .
              )
         (read-path-or-uri)]
        [(and (char=? c #\.)
              (peek 1)
              (char=? (peek 1) #\.)
              (peek 2)
              (char=? (peek 2) #\/)  ; Must be ../ not ...
              )
         (read-path-or-uri)]
        ;; Multi-char operators
        [(ormap (lambda (op)
                  (and (equal? (peek-string (string-length op)) op)
                       (begin (emit! 'OP op) (advance! (string-length op)) #t)))
                multi-ops)
         (void)]
        ;; Single-char operators
        [(member c single-ops)
         (define sym (string c))
         (define type (case c
                        [(#\{) 'LBRACE]
                        [(#\}) 'RBRACE]
                        [(#\[) 'LBRACK]
                        [(#\]) 'RBRACK]
                        [(#\() 'LPAREN]
                        [(#\)) 'RPAREN]
                        [(#\;) 'SEMI]
                        [(#\:) 'COLON]
                        [(#\,) 'COMMA]
                        [(#\.) 'DOT]
                        [(#\@) 'AT]
                        [(#\=) 'EQ]
                        [else 'OP]))
         (emit! type sym)
         (advance!)]
        ;; Antiquotation ${...} in attribute names
        [(and (char=? c #\$) (peek 1) (char=? (peek 1) #\{))
         (advance! 2)  ; skip ${
         (emit! 'DOLLAR-BRACE "${")
         ;; Tokenize until matching }
         (define interp-tokens (tokenize-until-brace))
         (emit! 'INTERP-EXPR interp-tokens)]
        ;; Identifier or keyword
        [(or (char-alphabetic? c) (char=? c #\_))
         (read-identifier)]
        [else
         (error (format "Unexpected character: ~a at position ~a" c pos))])))

  ;; Main tokenization loop
  (let loop ()
    (skip-whitespace-and-comments)
    (when (peek)
      (read-token)
      (loop)))

  (reverse tokens))

;; =============================================================================
;; PARSER
;; =============================================================================

;; Parser state
(struct parser-state (tokens pos) #:transparent #:mutable)

(define (make-parser tokens)
  (parser-state tokens 0))

(define (current-token ps)
  (if (< (parser-state-pos ps) (length (parser-state-tokens ps)))
      (list-ref (parser-state-tokens ps) (parser-state-pos ps))
      #f))

(define (peek-token ps [offset 0])
  (define idx (+ (parser-state-pos ps) offset))
  (if (< idx (length (parser-state-tokens ps)))
      (list-ref (parser-state-tokens ps) idx)
      #f))

(define (advance-parser! ps)
  (set-parser-state-pos! ps (add1 (parser-state-pos ps))))

(define (expect ps type [val #f])
  (define tok (current-token ps))
  (cond
    [(not tok) (error (format "Unexpected EOF, expected ~a" type))]
    [(not (eq? (token-type tok) type))
     (error (format "Expected ~a but got ~a: ~a" type (token-type tok) (token-value tok)))]
    [(and val (not (equal? (token-value tok) val)))
     (error (format "Expected ~a but got ~a" val (token-value tok)))]
    [else (advance-parser! ps) tok]))

(define (match-token? ps type [val #f])
  (define tok (current-token ps))
  (and tok
       (eq? (token-type tok) type)
       (or (not val) (equal? (token-value tok) val))))

(define (try-consume ps type [val #f])
  (if (match-token? ps type val)
      (begin (advance-parser! ps) #t)
      #f))

;; Main parse function
(define (parse-expr ps)
  (parse-impl ps))

;; Implication -> has lowest precedence
(define (parse-impl ps)
  (define left (parse-or ps))
  (if (match-token? ps 'OP "->")
      (begin
        (advance-parser! ps)
        (nix-binop "->" left (parse-impl ps))) ; right associative
      left))

(define (parse-or ps)
  (let loop ([left (parse-and ps)])
    (if (match-token? ps 'OP "||")
        (begin
          (advance-parser! ps)
          (loop (nix-binop "||" left (parse-and ps))))
        left)))

(define (parse-and ps)
  (let loop ([left (parse-eq ps)])
    (if (match-token? ps 'OP "&&")
        (begin
          (advance-parser! ps)
          (loop (nix-binop "&&" left (parse-eq ps))))
        left)))

(define (parse-eq ps)
  (let loop ([left (parse-cmp ps)])
    (cond
      [(match-token? ps 'OP "==")
       (advance-parser! ps)
       (loop (nix-binop "==" left (parse-cmp ps)))]
      [(match-token? ps 'OP "!=")
       (advance-parser! ps)
       (loop (nix-binop "!=" left (parse-cmp ps)))]
      [else left])))

(define (parse-cmp ps)
  (let loop ([left (parse-update ps)])
    (cond
      [(match-token? ps 'OP "<")
       (advance-parser! ps)
       (loop (nix-binop "<" left (parse-update ps)))]
      [(match-token? ps 'OP ">")
       (advance-parser! ps)
       (loop (nix-binop ">" left (parse-update ps)))]
      [(match-token? ps 'OP "<=")
       (advance-parser! ps)
       (loop (nix-binop "<=" left (parse-update ps)))]
      [(match-token? ps 'OP ">=")
       (advance-parser! ps)
       (loop (nix-binop ">=" left (parse-update ps)))]
      [else left])))

(define (parse-update ps)
  (define left (parse-not ps))
  (if (match-token? ps 'OP "//")
      (begin
        (advance-parser! ps)
        (nix-binop "//" left (parse-update ps))) ; right associative
      left))

(define (parse-not ps)
  (if (match-token? ps 'OP "!")
      (begin
        (advance-parser! ps)
        (nix-unop "!" (parse-not ps)))
      (parse-concat ps)))

(define (parse-concat ps)
  (define left (parse-add ps))
  (if (match-token? ps 'OP "++")
      (begin
        (advance-parser! ps)
        (nix-binop "++" left (parse-concat ps))) ; right associative
      left))

(define (parse-add ps)
  (let loop ([left (parse-mul ps)])
    (cond
      [(match-token? ps 'OP "+")
       (advance-parser! ps)
       (loop (nix-binop "+" left (parse-mul ps)))]
      [(match-token? ps 'OP "-")
       (advance-parser! ps)
       (loop (nix-binop "-" left (parse-mul ps)))]
      [else left])))

(define (parse-mul ps)
  (let loop ([left (parse-unary ps)])
    (cond
      [(match-token? ps 'OP "*")
       (advance-parser! ps)
       (loop (nix-binop "*" left (parse-unary ps)))]
      [(match-token? ps 'OP "/")
       ;; Be careful: / could be path
       (advance-parser! ps)
       (loop (nix-binop "/" left (parse-unary ps)))]
      [else left])))

(define (parse-unary ps)
  (if (match-token? ps 'OP "-")
      (begin
        (advance-parser! ps)
        (nix-unop "-" (parse-unary ps)))
      (parse-has-attr ps)))

(define (parse-has-attr ps)
  (define left (parse-app ps))
  (if (match-token? ps 'OP "?")
      (begin
        (advance-parser! ps)
        (nix-has-attr left (parse-attrpath ps)))
      left))

(define (parse-app ps)
  ;; Function application - left associative
  (let loop ([func (parse-select ps)])
    (if (can-start-expr? ps)
        (loop (nix-call func (parse-select ps)))
        func)))

(define (can-start-expr? ps)
  ;; Check if current token can start an expression (for function application)
  (define tok (current-token ps))
  (and tok
       (member (token-type tok)
               '(ID INT FLOAT STRING INTERP-STRING INTERP-EXPR DOLLAR-BRACE
                 BOOL NULL PATH INTERP-PATH URI LBRACE LBRACK LPAREN KEYWORD))
       ;; Don't consume keywords that are continuations
       (not (and (eq? (token-type tok) 'KEYWORD)
                 (member (token-value tok) '("then" "else" "in" "or"))))))

(define (parse-select ps)
  (let loop ([expr (parse-primary ps)])
    (cond
      [(match-token? ps 'DOT)
       (advance-parser! ps)
       (define path (parse-attrpath ps))
       (if (and (match-token? ps 'KEYWORD "or"))
           (begin
             (advance-parser! ps)
             (loop (nix-select expr path (parse-select ps))))
           (loop (nix-select expr path #f)))]
      [else expr])))

(define (parse-attrpath ps)
  ;; Parse attribute path (a.b.c or a."b".${c})
  (define parts '())
  (let loop ()
    (define tok (current-token ps))
    (cond
      [(match-token? ps 'ID)
       (set! parts (cons (token-value tok) parts))
       (advance-parser! ps)
       (when (match-token? ps 'DOT)
         (advance-parser! ps)
         (loop))]
      [(match-token? ps 'STRING)
       (set! parts (cons (token-value tok) parts))
       (advance-parser! ps)
       (when (match-token? ps 'DOT)
         (advance-parser! ps)
         (loop))]
      [(match-token? ps 'INTERP-STRING)
       (set! parts (cons (parse-interp-string (token-value tok)) parts))
       (advance-parser! ps)
       (when (match-token? ps 'DOT)
         (advance-parser! ps)
         (loop))]
      [(match-token? ps 'INTERP-EXPR)
       ;; ${expr} as attribute name
       (set! parts (cons (nix-interp (parse-tokens (token-value tok))) parts))
       (advance-parser! ps)
       (when (match-token? ps 'DOT)
         (advance-parser! ps)
         (loop))]
      [(match-token? ps 'DOLLAR-BRACE)
       ;; Skip the DOLLAR-BRACE token (the INTERP-EXPR should follow)
       (advance-parser! ps)
       (loop)]
      [else (void)]))
  (reverse parts))

(define (parse-interp-string parts)
  ;; Convert interpolated string parts to nix-string
  (nix-string
   (map (lambda (p)
          (if (and (pair? p) (eq? (car p) 'INTERP))
              (nix-interp (parse-tokens (cdr p)))
              p))
        parts)))

(define (parse-interp-path parts)
  ;; Convert interpolated path parts to nix-path with interpolation
  ;; Nix treats paths with interpolation as strings that get coerced to paths
  (nix-path
   (nix-string
    (map (lambda (p)
           (if (and (pair? p) (eq? (car p) 'INTERP))
               (nix-interp (parse-tokens (cdr p)))
               p))
         parts))))

(define (parse-tokens toks)
  ;; Parse a list of tokens as an expression
  (define ps (make-parser toks))
  (parse-expr ps))

(define (parse-primary ps)
  (define tok (current-token ps))
  (unless tok (error "Unexpected EOF"))

  (case (token-type tok)
    [(ID)
     (define name (token-value tok))
     (define next (peek-token ps 1))
     ;; Check for simple lambda: x: body
     (if (and next (eq? (token-type next) 'COLON))
         (parse-simple-lambda ps)
         (begin (advance-parser! ps) (nix-var name)))]

    [(INT) (advance-parser! ps) (nix-int (token-value tok))]
    [(FLOAT) (advance-parser! ps) (nix-float (token-value tok))]
    [(BOOL) (advance-parser! ps) (nix-bool (token-value tok))]
    [(NULL) (advance-parser! ps) (nix-null)]
    [(PATH) (advance-parser! ps) (nix-path (token-value tok))]
    [(URI) (advance-parser! ps) (nix-uri (token-value tok))]

    [(INTERP-PATH)
     ;; Path with interpolation - convert to a nix-string with path semantics
     (advance-parser! ps)
     (parse-interp-path (token-value tok))]

    [(STRING)
     (advance-parser! ps)
     (nix-string (list (token-value tok)))]

    [(INTERP-STRING)
     (advance-parser! ps)
     (parse-interp-string (token-value tok))]

    [(LBRACK) (parse-list ps)]
    [(LPAREN) (parse-paren ps)]
    [(LBRACE) (parse-set-or-lambda ps)]

    [(KEYWORD)
     (case (token-value tok)
       [("let") (parse-let ps)]
       [("if") (parse-if ps)]
       [("with") (parse-with ps)]
       [("assert") (parse-assert ps)]
       [("rec") (parse-rec-set ps)]
       [else (error (format "Unexpected keyword: ~a" (token-value tok)))])]

    [else (error (format "Unexpected token: ~a ~a" (token-type tok) (token-value tok)))]))

(define (parse-simple-lambda ps)
  ;; x: body
  (define name (token-value (current-token ps)))
  (advance-parser! ps) ; skip ID
  (expect ps 'COLON)
  (nix-lambda (param-id name) (parse-expr ps)))

(define (parse-list ps)
  (expect ps 'LBRACK)
  (define elems '())
  (let loop ()
    (unless (match-token? ps 'RBRACK)
      (set! elems (cons (parse-select ps) elems))
      (loop)))
  (expect ps 'RBRACK)
  (nix-list (reverse elems)))

(define (parse-paren ps)
  (expect ps 'LPAREN)
  (define expr (parse-expr ps))
  (expect ps 'RPAREN)
  expr)

(define (parse-set-or-lambda ps)
  ;; Could be: { }, { a = ...; }, { a, b, ... }: body, or { a ? default }: body
  (expect ps 'LBRACE)

  ;; Look ahead to determine if this is a lambda or a set
  (define is-lambda (is-lambda-pattern? ps))

  (if is-lambda
      (parse-lambda-from-brace ps)
      (parse-set-body ps)))

(define (is-lambda-pattern? ps)
  ;; Scan ahead to see if this looks like a lambda pattern
  ;; Lambda: { } : or { id, or { id ? or { id } : or { ... }
  (define saved-pos (parser-state-pos ps))
  (define result
    (let loop ([depth 0])
      (define tok (current-token ps))
      (cond
        [(not tok) #f]
        [(and (eq? (token-type tok) 'RBRACE) (= depth 0))
         ;; Check what comes after }
         (advance-parser! ps)
         (define next (current-token ps))
         (and next
              (or (eq? (token-type next) 'COLON)
                  (eq? (token-type next) 'AT)))]
        [(eq? (token-type tok) 'LBRACE)
         (advance-parser! ps)
         (loop (add1 depth))]
        [(eq? (token-type tok) 'RBRACE)
         (advance-parser! ps)
         (loop (sub1 depth))]
        [(eq? (token-type tok) 'EQ)
         ;; If we see = at depth 0 before finding the pattern end, it's a set
         (if (= depth 0) #f (begin (advance-parser! ps) (loop depth)))]
        [(eq? (token-type tok) 'SEMI)
         ;; Semicolon at depth 0 means set
         (if (= depth 0) #f (begin (advance-parser! ps) (loop depth)))]
        [else
         (advance-parser! ps)
         (loop depth)])))
  (set-parser-state-pos! ps saved-pos)
  result)

(define (parse-lambda-from-brace ps)
  ;; We're inside { already consumed
  ;; Parse pattern: { a, b ? default, ... }
  (define args '())
  (define variadic #f)

  (let loop ()
    (cond
      [(match-token? ps 'RBRACE) (void)] ; done
      [(match-token? ps 'OP "...")
       (advance-parser! ps)
       (set! variadic #t)
       (try-consume ps 'COMMA)
       (loop)]
      [(match-token? ps 'ID)
       (define name (token-value (current-token ps)))
       (advance-parser! ps)
       (define default
         (if (match-token? ps 'OP "?")
             (begin (advance-parser! ps) (parse-expr ps))
             #f))
       (set! args (cons (param-arg name default) args))
       (try-consume ps 'COMMA)
       (loop)]
      [(match-token? ps 'COMMA)
       (advance-parser! ps)
       (loop)]
      [else (error (format "Unexpected in lambda pattern: ~a" (current-token ps)))]))

  (expect ps 'RBRACE)

  ;; Check for @name or name@
  (define at-name #f)
  (when (match-token? ps 'AT)
    (advance-parser! ps)
    (set! at-name (token-value (expect ps 'ID))))

  (expect ps 'COLON)
  (define body (parse-expr ps))
  (nix-lambda (param-set (reverse args) variadic at-name) body))

(define (parse-set-body ps)
  ;; Parse set bindings (we're after {)
  (define bindings '())

  (let loop ()
    (cond
      [(match-token? ps 'RBRACE) (void)]
      [(match-token? ps 'KEYWORD "inherit")
       (set! bindings (cons (parse-inherit ps) bindings))
       (loop)]
      [else
       (define path (parse-attrpath ps))
       (expect ps 'EQ)
       (define val (parse-expr ps))
       (expect ps 'SEMI)
       (set! bindings (cons (binding path val) bindings))
       (loop)]))

  (expect ps 'RBRACE)
  (nix-set (reverse bindings)))

(define (parse-inherit ps)
  (expect ps 'KEYWORD "inherit")
  (cond
    [(match-token? ps 'LPAREN)
     (advance-parser! ps)
     (define from (parse-expr ps))
     (expect ps 'RPAREN)
     (define names '())
     (let loop ()
       (when (match-token? ps 'ID)
         (set! names (cons (token-value (current-token ps)) names))
         (advance-parser! ps)
         (loop)))
     (expect ps 'SEMI)
     (inherit-from-binding from (reverse names))]
    [else
     (define names '())
     (let loop ()
       (when (match-token? ps 'ID)
         (set! names (cons (token-value (current-token ps)) names))
         (advance-parser! ps)
         (loop)))
     (expect ps 'SEMI)
     (inherit-binding (reverse names))]))

(define (parse-let ps)
  (expect ps 'KEYWORD "let")
  (define bindings '())

  (let loop ()
    (cond
      [(match-token? ps 'KEYWORD "in") (void)]
      [(match-token? ps 'KEYWORD "inherit")
       (set! bindings (cons (parse-inherit ps) bindings))
       (loop)]
      [else
       (define path (parse-attrpath ps))
       (expect ps 'EQ)
       (define val (parse-expr ps))
       (expect ps 'SEMI)
       (set! bindings (cons (binding path val) bindings))
       (loop)]))

  (expect ps 'KEYWORD "in")
  (nix-let (reverse bindings) (parse-expr ps)))

(define (parse-if ps)
  (expect ps 'KEYWORD "if")
  (define cond-expr (parse-expr ps))
  (expect ps 'KEYWORD "then")
  (define then-expr (parse-expr ps))
  (expect ps 'KEYWORD "else")
  (define else-expr (parse-expr ps))
  (nix-if cond-expr then-expr else-expr))

(define (parse-with ps)
  (expect ps 'KEYWORD "with")
  (define env (parse-expr ps))
  (expect ps 'SEMI)
  (nix-with env (parse-expr ps)))

(define (parse-assert ps)
  (expect ps 'KEYWORD "assert")
  (define cond-expr (parse-expr ps))
  (expect ps 'SEMI)
  (nix-assert cond-expr (parse-expr ps)))

(define (parse-rec-set ps)
  (expect ps 'KEYWORD "rec")
  (expect ps 'LBRACE)
  (define bindings '())

  (let loop ()
    (cond
      [(match-token? ps 'RBRACE) (void)]
      [(match-token? ps 'KEYWORD "inherit")
       (set! bindings (cons (parse-inherit ps) bindings))
       (loop)]
      [else
       (define path (parse-attrpath ps))
       (expect ps 'EQ)
       (define val (parse-expr ps))
       (expect ps 'SEMI)
       (set! bindings (cons (binding path val) bindings))
       (loop)]))

  (expect ps 'RBRACE)
  (nix-rec-set (reverse bindings)))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(define (parse-nix-structure-strict str)
  (define tokens (tokenize str))
  (when (empty? tokens) (error "Empty input"))
  (define ps (make-parser tokens))
  (define result (parse-expr ps))
  (when (current-token ps)
    (error (format "Trailing content: ~a" (current-token ps))))
  result)

(define (parse-nix-structure str)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (exn-message e))
                               (nix-var str))])
    (parse-nix-structure-strict str)))
