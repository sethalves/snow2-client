(define-library (seth snow2 r7rs-library)
  (export r7rs-get-import-decls
          r7rs-get-imported-library-names
          is-system-import?
          ;; r7rs-get-exports-from-imports
          r7rs-get-exports-from-import-set
          r7rs-get-referenced-symbols
          r7rs-library-file->sexp
          )
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme file)
          (snow filesys)
          ;; (snow srfi-13-strings)
          (snow srfi-95-sort)
          ;; (seth srfi-69-hash-tables)
          (seth snow2 types)
          (seth snow2 utils)
          )

  (cond-expand
   (chibi (import (only (srfi 1) filter find)))
   (else (import (srfi 1))))

  (begin

    (define (r7rs-explode-cond-expand r7rs-lib)
      (let loop ((r7rs-lib r7rs-lib)
                 (result '()))
        (cond ((null? r7rs-lib) result)
              (else
               (let ((term (car r7rs-lib)))
                 (cond ((and (pair? term)
                             (eq? (car term) 'cond-expand))
                        (let* ((ce-terms (cdr term))
                               (childs (map cdr ce-terms))
                               (childs-flat (apply append childs)))
                          (loop (cdr r7rs-lib) (append result childs-flat))))
                       (else
                        (loop (cdr r7rs-lib)
                              (append result (list term))))))))))


    (define (r7rs-drop-body r7rs-lib)
      ;; return r7rs-lib with (begin ...) snipped out
      (filter
       (lambda (term)
         (not (and (pair? term) (eq? (car term) 'begin))))
       r7rs-lib))


    (define (r7rs-body r7rs-lib)
      ;; return (begin ...)
      (find
       (lambda (term)
         (and (pair? term) (eq? (car term) 'begin)))
       r7rs-lib))


    (define (r7rs-extract-im/export r7rs-lib type)
      (let loop ((r7rs-lib r7rs-lib)
                 (result '()))
        (cond ((null? r7rs-lib) result)
              (else
               (let ((term (car r7rs-lib)))
                 (cond ((and (pair? term) (eq? (car term) type))
                        (let ((childs (cdr term)))
                          (loop (cdr r7rs-lib) (append result childs))))
                       (else
                        (loop (cdr r7rs-lib) result))))))))

    (define (uniq lst)
      (cond ((null? lst) lst)
            ((member (car lst) (cdr lst)) (uniq (cdr lst)))
            (else (cons (car lst) (uniq (cdr lst))))))


    (define (is-system-import? lib-name)
      (and (pair? lib-name)
           (memq (car lib-name)
                 '(scheme scheme chibi r7rs gauche sagittarius
                          ports tcp rnrs use openssl udp posix
                          srfi chicken ssax sxml sxpath txpath
                          sxpath-lolevel text md5 rfc math sha1 sha2
                          util memcached matchable match
                          extras http-client uri-generic intarweb
                          message-digest file z3 base64 hmac
                          binary input-parse
                          srfi-27 srfi-95))))


    (define (r7rs-filter-known-imports r7rs-imports)
      (filter
       (lambda (r7rs-import)
         (not (is-system-import? r7rs-import)))
       r7rs-imports))


    (define (r7rs-import-set->libs r7rs-import)
      (cond ((not (pair? r7rs-import))
             (display "Warning: unexpected import form: ")
             (write r7rs-import)
             (newline)
             r7rs-import)
            ((eq? (car r7rs-import) 'only)
             (r7rs-import-set->libs (cadr r7rs-import)))
            ((eq? (car r7rs-import) 'prefix)
             (r7rs-import-set->libs (cadr r7rs-import)))
            (else r7rs-import)))


    (define (r7rs-library-file->sexp filename)
      (let* ((p (open-input-file filename))
             (lib-sexp (read p)))
        (close-input-port p)
        lib-sexp))


    (define (r7rs-get-import-decls lib-sexp)
      ;; extract a list of import-delcs from the (import ...) statements
      ;; in lib-sexp
      (let* ((lib-no-begin (r7rs-drop-body lib-sexp))
             (lib-sans-ce (r7rs-explode-cond-expand lib-no-begin)))
        (uniq (r7rs-extract-im/export lib-sans-ce 'import))))


    (define (r7rs-get-imported-library-names lib-sexp)
      ;; return a list of library-names that may be imported by this library
      (let* ((lib-imports-all (r7rs-get-import-decls lib-sexp))
             (lib-imports-clean (map r7rs-import-set->libs lib-imports-all)))
        (r7rs-filter-known-imports (uniq lib-imports-clean))))


    (define (r7rs-get-library-exports filename)
      (let* ((p (open-input-file filename))
             (r7rs-lib (read p))
             (r7rs-no-begin (r7rs-drop-body r7rs-lib))
             (r7rs-exports (r7rs-extract-im/export r7rs-no-begin 'export)))
        (close-input-port p)
        r7rs-exports))


    (define (r7rs-get-exports-from-import-set repositories import-set)
      (cond ((not (list? import-set))
             (error "import-set isn't a list"))
            ((null? import-set)
             (error "import-set is empty"))
            ((eq? (car import-set) 'only)
             (let ((lib (car (r7rs-import-set->libs (list (cadr import-set))))))
               (values lib (cddr import-set))))
            ((eq? (car import-set) 'prefix)
             (let ((prefix (symbol->string (caddr import-set))))
               (let-values (((sub-lib sub-identifiers)
                             (r7rs-get-exports-from-import-set
                              repositories (cadr import-set))))
                 (values sub-lib
                         (map (lambda (identifier)
                                (string->symbol
                                 (string-append
                                  prefix (symbol->string identifier))))
                              sub-identifiers)))))
            ((eq? (car import-set) 'prefix)
             (error "write this"))
            ((eq? (car import-set) 'rename)
             (error "write this"))
            (else
             (let* ((local-repos (filter snow2-repository-local repositories))
                    (libs (find-libraries-by-name local-repos import-set))
                    (libs-len (length libs)))
               (cond ((= libs-len 0)
                      (values #f '()))
                     ((> libs-len 1)
                      (display "library ")
                      (write import-set)
                      (display " found in more than one repository.\n")
                      (values #f '()))
                     (else
                      (let* ((lib (car libs))
                             (lib-pkg (snow2-library-package lib))
                             (lib-repo (snow2-package-repository lib-pkg)))
                        (cond (lib-repo
                               (values import-set
                                       (r7rs-get-library-exports
                                        (snow-combine-filename-parts
                                         (local-repository->in-fs-lib-path
                                          lib-repo lib)))))
                              (else
                               (display "didn't find repository for ")
                               (write (snow2-library-name (car libs)))
                               (display ", perhaps need a re-package?\n")
                               (values #f '()))))))))))


    (define (flatten lst)
      (cond
       ((null? lst) '())
       ((not (pair? lst)) (list lst))
       ((pair? lst)
        (append (flatten (car lst)) (flatten (cdr lst))))
       (else
        (cons (car lst) (flatten (cdr lst))))))


    (define (r7rs-get-referenced-symbols lib-sexp)
      (let ((body (r7rs-body lib-sexp)))
        (cond (body
               (sort
                (uniq (filter symbol? (flatten body)))
                (lambda (a b)
                  (string-ci<? (symbol->string a) (symbol->string b)))))
              (else '()))))


    ))
