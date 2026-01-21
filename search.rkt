#lang racket

(require json)

(provide search-packages
         package-result
         package-result-attr-path
         package-result-pname
         package-result-version
         package-result-description
         format-package-name
         ;; Options search
         search-options
         option-result
         option-result-name
         option-result-type
         option-result-default
         option-result-description)

;; --- DATA STRUCTURES ---

;; Represents a search result from nixpkgs
(struct package-result (attr-path pname version description) #:transparent)

;; --- SEARCH FUNCTIONS ---

;; Main search function - tries nix search, falls back to nix-env
;; Returns (values results error-message)
;; results is a list of package-result, error-message is #f on success
(define (search-packages query)
  (with-handlers ([exn:fail? (lambda (e)
                               (values '() (exn-message e)))])
    (define-values (results err) (nix-search-json query))
    (if (null? results)
        (nix-env-fallback query)
        (values results err))))

;; Primary search: nix search nixpkgs <query> --json
(define (nix-search-json query)
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f
                (find-executable-path "nix")
                "search" "nixpkgs" query "--json"))

  (close-output-port stdin)

  ;; Read stdout and stderr in parallel to avoid deadlock
  (define json-str "")
  (define err-str "")
  (define stdout-thread (thread (lambda () (set! json-str (port->string stdout)))))
  (define stderr-thread (thread (lambda () (set! err-str (port->string stderr)))))

  (thread-wait stdout-thread)
  (thread-wait stderr-thread)
  (subprocess-wait proc)

  (define status (subprocess-status proc))
  (close-input-port stdout)
  (close-input-port stderr)

  (cond
    [(and (zero? status) (non-empty-string? json-str))
     (values (parse-nix-search-json json-str) #f)]
    [(non-empty-string? err-str)
     (values '() (string-trim err-str))]
    [else
     (values '() "No results found")]))

;; Parse JSON output from nix search
;; Format: { "legacyPackages.x86_64-linux.firefox": { "pname": "...", "version": "...", "description": "..." } }
(define (parse-nix-search-json json-str)
  (define data (string->jsexpr json-str))
  (for/list ([(attr-key pkg-hash) (in-hash data)])
    (define attr-path (symbol->string attr-key))
    ;; Extract the package name from the full attr path
    (define short-name (extract-package-name attr-path))
    (package-result attr-path
                    (hash-ref pkg-hash 'pname short-name)
                    (hash-ref pkg-hash 'version "")
                    (hash-ref pkg-hash 'description ""))))

;; Extract package name from attr path like "legacyPackages.x86_64-linux.firefox"
(define (extract-package-name attr-path)
  (define parts (string-split attr-path "."))
  (if (>= (length parts) 3)
      (string-join (drop parts 2) ".")  ;; e.g., "firefox" or "python310Packages.numpy"
      (last parts)))

;; Fallback search: nix-env -qaP '*<query>*'
(define (nix-env-fallback query)
  (with-handlers ([exn:fail? (lambda (e)
                               (values '() (exn-message e)))])
    (define-values (proc stdout stdin stderr)
      (subprocess #f #f #f
                  (find-executable-path "nix-env")
                  "-qaP" (format "*~a*" query)))

    (close-output-port stdin)

    ;; Read stdout and stderr in parallel to avoid deadlock
    (define output "")
    (define err-str "")
    (define stdout-thread (thread (lambda () (set! output (port->string stdout)))))
    (define stderr-thread (thread (lambda () (set! err-str (port->string stderr)))))

    (thread-wait stdout-thread)
    (thread-wait stderr-thread)
    (subprocess-wait proc)

    (close-input-port stdout)
    (close-input-port stderr)

    (if (non-empty-string? output)
        (values (parse-nix-env-output output) #f)
        (values '() (if (non-empty-string? err-str)
                        (string-trim err-str)
                        "No results found")))))

;; Parse output from nix-env -qaP
;; Format: "nixpkgs.firefox  firefox-123.0"
(define (parse-nix-env-output output)
  (for/list ([line (string-split output "\n")]
             #:when (non-empty-string? (string-trim line)))
    (define parts (string-split (string-trim line)))
    (cond
      [(>= (length parts) 2)
       (define attr-path (first parts))
       (define name-version (second parts))
       ;; Try to split name-version at the last hyphen before version number
       (define-values (pname version) (split-name-version name-version))
       (package-result attr-path pname version "")]
      [(= (length parts) 1)
       (package-result (first parts) (first parts) "" "")]
      [else
       (package-result "" "" "" "")])))

;; Split "firefox-123.0" into ("firefox" "123.0")
(define (split-name-version str)
  (define match (regexp-match #px"^(.+)-([0-9].*)$" str))
  (if match
      (values (second match) (third match))
      (values str "")))

;; Format package name for display
(define (format-package-name pkg)
  (extract-package-name (package-result-attr-path pkg)))

;; =============================================================================
;; NIXOS OPTIONS SEARCH
;; =============================================================================

;; Represents a NixOS option search result
(struct option-result (name type default description) #:transparent)

;; Search NixOS/Home Manager options using manix
;; source can be "nixos_options" or "hm_options"
;; Returns (values results error-message)
(define (search-options query [source "nixos_options"])
  (with-handlers ([exn:fail? (lambda (e)
                               (values '() (exn-message e)))])
    ;; Try manix first (if available)
    (define-values (results1 err1) (manix-search query source))
    (if (not (null? results1))
        (values results1 err1)
        ;; Fall back to error message
        (nixos-options-web-search query))))

;; Search using manix (fast, comprehensive)
;; source: "nixos_options" or "hm_options" (not currently used, manix searches all)
(define (manix-search query [source "nixos_options"])
  (define manix-path (find-executable-path "manix"))
  (cond
    [(not manix-path)
     (values '() #f)]  ;; manix not found, try fallback
    [else
     (define-values (proc stdout stdin stderr)
       (subprocess #f #f #f manix-path query))

     (close-output-port stdin)

     (define output "")
     (define err-str "")
     (define stdout-thread (thread (lambda () (set! output (port->string stdout)))))
     (define stderr-thread (thread (lambda () (set! err-str (port->string stderr)))))

     (thread-wait stdout-thread)
     (thread-wait stderr-thread)
     (subprocess-wait proc)

     (close-input-port stdout)
     (close-input-port stderr)

     (if (non-empty-string? output)
         (values (parse-manix-output output query) #f)
         (values '() #f))]))

;; Parse manix output
;; Format:
;; # option.name (type)
;; description...
(define (parse-manix-output output query)
  (define lines (string-split output "\n"))
  (define results '())
  (define current-name #f)
  (define current-type "")
  (define current-desc '())

  (define (flush!)
    (when (and current-name
               (or (string-contains? (string-downcase current-name) (string-downcase query))
                   (ormap (lambda (d) (string-contains? (string-downcase d) (string-downcase query)))
                          current-desc)))
      (set! results (cons (option-result current-name
                                         current-type
                                         ""
                                         (string-join (reverse current-desc) " "))
                          results)))
    (set! current-name #f)
    (set! current-type "")
    (set! current-desc '()))

  (for ([line lines])
    (cond
      [(string-prefix? line "# ")
       (flush!)
       ;; Parse "# option.name (type)" or "# option.name"
       (define match (regexp-match #px"^# ([^ ]+)(?: \\((.+)\\))?$" line))
       (when match
         (set! current-name (second match))
         (set! current-type (or (third match) "")))]
      [(and current-name (non-empty-string? (string-trim line)))
       (set! current-desc (cons (string-trim line) current-desc))]))

  (flush!)
  (reverse results))

;; Fallback when manix is not available
(define (nixos-options-web-search query)
  (values '() "Options search requires manix. Run 'direnv reload' or add manix to your shell."))
