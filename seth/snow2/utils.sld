(define-library (seth snow2 utils)
  (export get-repository
          caching-get-repository
          read-repository
          ;; get-repositories
          ;; get-repositories-and-siblings
          make-repository-iterator
          get-next-repository
          get-current-repositories
          for-each-repository
          add-repository-to-hash

          find-packages-with-library
          snow2-packages-libraries
          get-snow2-repo-name
          find-package-with-library
          find-packages-with-libraries
          find-libraries-for-steps
          gather-depends

          snow2-package-absolute-url
          snow2-package-absolute-path
          repo-path->file-path
          repo-url->file-url

          package-from-sexp
          depend->sexp
          sibling->sexp
          library->sexp
          package->sexp
          repository->sexp
          refresh-package-from-filename
          find-libraries-by-name
          lib-sexp->name

          copy-snow2-library

          local-repository->in-fs-index-path
          local-repository->in-fs-index-filename
          local-repository->in-fs-html-path
          local-repository->in-fs-html-filename
          local-repository->in-fs-css-path
          local-repository->in-fs-css-filename
          local-repository->in-fs-tgz-path
          local-repository->in-fs-tgz-filename
          local-repository->in-fs-lib-path
          local-repository->in-fs-lib-filename

          find-matching-repository-in-list

          sanity-check-repository
          sanity-check-package

          repository->html

          report-unfound-libs
          is-system-import?
          )

  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme lazy)
          (only (scheme r5rs) string-ci<?)
          (scheme process-context)
          (srfi 95)
          (snow assert)
          (seth cout))
  (cond-expand
   (chibi (import (only (srfi 1) fold lset-intersection last drop-right find)
                  (only (chibi) read)))
   (else (import (scheme read)
                 (srfi 1))))
  (cond-expand
   (chibi (import (chibi filesystem)))
   (else))
  (import (snow extio)
          (except (srfi 13)
                  string-copy string-map string-for-each
                  string-fill! string-copy! string->list
                  string-upcase string-downcase string-hash)
          (srfi 14)
          (srfi 69)
          (srfi 29)
          (snow filesys)
          (prefix (seth http) http-)
          (seth string-read-write)
          (seth uri)
          (seth snow2 types)
          (seth xml sxml-serializer)
          (seth deep-copy))

  (begin

    (define (list-replace-last lst new-elt)
      ;; what's the srfi-1 one-liner for this?
      (reverse (cons new-elt (cdr (reverse lst)))))


    (define (is-system-import? lib-name)
      ;; XXX generate this list with (features) ?

      (member lib-name
              '((base64)
                (binary io) (chibi) (chibi base64) (chibi char-set)
                (chibi char-set ascii) (chibi crypto md5)
                (chibi filesystem) (chibi io) (chibi mime)
                (chibi net) (chibi net http) (chibi process)
                (chibi quoted-printable) (chibi show base) (chibi string)
                (chicken) (extras) (files) (file util)
                (foment base) (gauche base) (gauche fileutil)
                (gauche net) (gauche process) (gauche uvector)
                (gauche vport) (hmac) (http-client) (input-parse)
                (intarweb) (match) (matchable) (math) (math hash)
                (md5) (message-digest) (openssl) (ports) (posix)
                (r7rs) (rfc base64) (rfc hmac) (rfc http) (rfc md5)
                (rfc mime) (rfc quoted-printable) (rfc sha)
                (rfc uri) (rfc zlib) (rnrs) (sagittarius)
                (sagittarius io) (sagittarius process)
                (sagittarius socket) (scheme base) (scheme case-lambda)
                (scheme char) (scheme cxr) (scheme eval) (scheme file)
                (scheme lazy) (scheme process-context) (scheme r5rs)
                (scheme read) (scheme time) (scheme write) (sha1) (sha2)
                (srfi 4) (ssax) (sxml ssax) (sxml sxpath) (sxpath)
                (sxpath-lolevel) (tcp) (text parse) (text sxml ssax)
                (text sxml sxpath) (txpath) (udp)
                (uri-generic) (util bytevector) (util digest)
                (util file) (util match) (z3)
                )))


    (define unfound-lib-hash (make-hash-table))

    (define (add-to-unfound-libs library-name)
      (if (not (is-system-import? library-name))
          (hash-table-set! unfound-lib-hash (write-to-string library-name) #t)))

    (define (report-unfound-libs)
      (let ((unfound (sort (hash-table-keys unfound-lib-hash) string<=?)))
        (if (> (length unfound) 0)
            (cout "couldn't find libraries: " unfound "\n"))))



    (define repository-hash (make-hash-table))

    (define (repository-hash-key repository)
      (snow-assert (snow2-repository? repository))
      (uri->hashtable-key
       (snow2-repository-url repository)))

    (define (url-in-repository-hash? repository-url)
      (snow-assert (uri-reference? repository-url))
      (hash-table-exists?
       repository-hash (uri->hashtable-key repository-url)))

    (define (get-repository-by-url repository-url)
      (snow-assert (uri-reference? repository-url))
      (hash-table-ref repository-hash (uri->hashtable-key repository-url)))

    (define (repository-in-hash? repository)
      (snow-assert (snow2-repository? repository))
      (hash-table-exists? repository-hash (repository-hash-key repository)))

    (define (replace-repository-in-hash old new)
      (snow-assert (snow2-repository? old))
      (snow-assert (snow2-repository? new))
      (hash-table-set! repository-hash (repository-hash-key old) new))

    (define (add-repository-to-hash repository-url repository)
      (hash-table-set!
       repository-hash (uri->hashtable-key repository-url)
       repository))



    (define (find-matching-repository-in-list needle hay-list)
      ;; see if any of the repositories in hay-list have the same
      ;; url as needle.
      (snow-assert (or (not needle) (snow2-repository? needle)))
      (snow-assert (list? hay-list))
      (let ((needle-key (repository-hash-key needle)))
        (find (lambda (local-repository)
                (equal?
                 (repository-hash-key local-repository)
                 needle-key))
              hay-list)))



    ;; due to the delayed downloading of repository indexes, the
    ;; list of repositories can change during a loop.
    (define-record-type <repository-iterator>
      (make-repository-iterator~ table)
      repository-iterator?
      (table repository-iterator-table))

    (define (make-repository-iterator)
      (make-repository-iterator~ (make-hash-table)))

    (define (get-next-repository iter)
      (snow-assert (repository-iterator? iter))
      (let loop ((current-repo-list (hash-table-values repository-hash)))
        (cond ((null? current-repo-list) #f)
              ((and
                (snow2-repository-local (car current-repo-list))
                (hash-table-exists?
                 (repository-iterator-table iter)
                 (uri->hashtable-key
                  (snow2-repository-local (car current-repo-list)))))
               ;; this iterator already returned this one
               (loop (cdr current-repo-list)))
              ((hash-table-exists?
                (repository-iterator-table iter)
                (uri->hashtable-key
                 (snow2-repository-url (car current-repo-list))))
               ;; this iterator already returned this one
               (loop (cdr current-repo-list)))
              (else
               ;; this iterator hasn't returned this one.
               (hash-table-set!
                (repository-iterator-table iter)
                (uri->hashtable-key
                 (snow2-repository-url (car current-repo-list)))
                #t)
               (car current-repo-list)))))

    (define (get-current-repositories)
      (hash-table-values repository-hash))


    (define (for-each-repository proc)
      (let ((iter (make-repository-iterator)))
        (let repo-loop ((repository (get-next-repository iter)))
          (cond (repository
                 (proc repository)
                 (repo-loop (get-next-repository iter)))))))


    (define (snow2-package-absolute-url/path repo-url package)
      (snow-assert (uri-reference? repo-url))
      (snow-assert (snow2-package? package))
      (let* ((pkg-url (snow2-package-url package)))
        (cond ((absolute-uri? pkg-url) pkg-url)
              ((and repo-url
                    (pair? (uri-path repo-url))
                    (eq? '/ (car (uri-path repo-url))))
               (uri-relative-to pkg-url repo-url))
              (else
               (update-uri
                pkg-url
                'path (append (uri-path repo-url) (uri-path pkg-url)))))))


    (define (snow2-package-absolute-url package)
      (snow-assert (snow2-package? package))
      ;; the url for a package may be relative to the repository's url.
      ;; this will return an absolute version.
      (let ((repository (snow2-package-repository package)))
        (cond ((not (snow2-repository-url repository)) #f)
              (else
               (snow2-package-absolute-url/path
                (snow2-repository-url repository) package)))))


    (define (snow2-package-absolute-path package)
      (snow-assert (snow2-package? package))
      ;; combine a local repsitory's in-filesystem path
      ;; with the package url
      (let ((repository (snow2-package-repository package)))
        (cond ((not (snow2-repository-local repository)) #f)
              (else
               (snow2-package-absolute-url/path
                (snow2-repository-local repository) package)))))


    (define (depend-from-sexp depend-sexp)
      (snow-assert (list? depend-sexp))
      ;; depend-sexp will be a library name, like (snow hello)
      depend-sexp)


    (define (depend->sexp depend)
      (snow-assert (list? depend))
      depend)


    (define (sibling-from-sexp sibling-sexp)
      ;; siblings look like
      ;;
      ;; (sibling
      ;;  (name "Snow Base Repository")
      ;;  (url "http://snow-repository.s3-website-us-east-1.amazonaws.com/")
      ;;  (trust 1.0))
      ;;
      ;; we save the trust value, but don't currently do anything with it.
      (snow-assert (eq? 'sibling (car sibling-sexp)))
      (let ((name (get-string-by-type sibling-sexp 'name #f))
            (url (get-string-by-type sibling-sexp 'url #f))
            (trust (get-number-by-type sibling-sexp 'trust 0.5)))
        (make-snow2-sibling name (uri-reference url) trust)))

    (define (sibling->sexp sibling)
      (snow-assert (snow2-sibling? sibling))
      `(sibling
        (name ,(snow2-sibling-name sibling))
        (url ,(uri->string (snow2-sibling-url sibling)))
        (trust ,(snow2-sibling-trust sibling))))


    (define (sibling->html sibling)
      (snow-assert (snow2-sibling? sibling))
      (let* ((name (snow2-sibling-name sibling))
             ;; adjust url to point to index.html rather than index.scm
             (scm-uri (snow2-sibling-url sibling))
             (scm-path (uri-path scm-uri))
             (html-path (drop-right scm-path 1))
             (html-uri (update-uri scm-uri 'path html-path))
             ;; trust level
             (trust (snow2-sibling-trust sibling)))
        `(html:p (html:a (@ (href ,(uri->string html-uri))) ,name)
                 " trust=" ,trust)))


    (define (library-from-sexp library-sexp)
      (snow-assert (list? library-sexp))
      ;; convert an s-exp into a library record
      (let ((name (get-list-by-type library-sexp 'name '()))
            (path (get-string-by-type library-sexp 'path #f))
            (depends-sexps (get-multi-args-by-type library-sexp 'depends '()))
            (version (get-string-by-type library-sexp 'version "1.0"))
            (homepage (get-args-by-type library-sexp 'homepage '()))
            (manual (get-args-by-type library-sexp 'manual '()))
            (maintainers (get-args-by-type library-sexp 'maintainers '()))
            (authors (get-args-by-type library-sexp 'authors '()))
            (description (get-args-by-type library-sexp 'description '()))
            (license (get-args-by-type library-sexp 'license '()))
            (use-for (get-args-by-type library-sexp 'use-for '(use-for final))))

        (cond ((not path) #f)
              (else
               (make-snow2-library
                name path
                (map depend-from-sexp depends-sexps)
                version
                homepage
                manual
                maintainers
                authors
                description
                license
                #f
                use-for)))))


    (define (copy-snow2-library library)
      (snow-assert (snow2-library? library))
      (make-snow2-library
       (deep-copy (snow2-library-name library))
       (deep-copy (snow2-library-path library))
       (deep-copy (snow2-library-depends library))
       (deep-copy (snow2-library-version library))
       (deep-copy (snow2-library-homepage library))
       (deep-copy (snow2-library-manual library))
       (deep-copy (snow2-library-maintainers library))
       (deep-copy (snow2-library-authors library))
       (deep-copy (snow2-library-description library))
       (deep-copy (snow2-library-license library))
       (snow2-library-package library)
       (deep-copy (snow2-library-use-for library))))


    (define (library->sexp library)
      (snow-assert (snow2-library? library))
      `(library
        (name ,(snow2-library-name library))
        (path ,(snow2-library-path library))
        (version ,(snow2-library-version library))
        (homepage ,@(snow2-library-homepage library))
        (manual ,@(snow2-library-manual library))
        (maintainers ,@(snow2-library-maintainers library))
        (authors ,@(snow2-library-authors library))
        (description ,@(snow2-library-description library))
        (license ,@(snow2-library-license library))
        (depends ,@(map depend->sexp (snow2-library-depends library)))
        (use-for ,@(snow2-library-use-for library))
        ))


    (define (library->html library)
      (snow-assert (snow2-library? library))
      (let* ((name (snow2-library-name library))
             (path (snow2-library-path library))
             (version (snow2-library-version library))
             (homepage (cond ((pair? (snow2-library-homepage library))
                              (car (snow2-library-homepage library)))
                             (else #f)))
             (manuals-en (assq 'en (snow2-library-manual library)))
             (manuals-else (assq 'else (snow2-library-manual library)))
             (manuals (cond (manuals-en (cdr manuals-en))
                            (manuals-else (cdr manuals-else))
                            (else '())))
             (maintainers
              (string-join
               (map write-to-string (snow2-library-maintainers library))
               ", "))
             (authors
              (string-join
               (map write-to-string (snow2-library-authors library))
               ", "))
             (description (snow2-library-description library))
             (license (snow2-library-license library))
             (depends
              (string-join
               (map display-to-string (snow2-library-depends library))
               " "))
             (use-for (snow2-library-use-for library)))

        `(html:li
          (html:p
           ,@(if homepage
                 `((html:a (@ (class "lib-a") (href ,homepage))
                           ,(display-to-string name)))
                 `())
           " -- "
           ,description)
          (html:p (@ (class "lib-details"))
                  "Version: " ,(display-to-string version))
          ,@(map
             (lambda (manual)
               `((html:p (@ (class "lib-details"))
                         (html:a (@ (href ,manual)) "manual"))))
             manuals)
          (html:p (@ (class "lib-details"))
                  "Authors: " ,(display-to-string authors))
          (html:p (@ (class "lib-details"))
                  "Maintainers: " ,(display-to-string maintainers))
          (html:p (@ (class "lib-details"))
                  "License: " ,(display-to-string license))
          (html:p (@ (class "lib-details"))
                  "Depends: " ,depends)
          (html:p (@ (class "lib-details"))
                  "Use-For: " ,(display-to-string use-for)))))


    (define (package-from-sexp package-sexp)
      (snow-assert (list? package-sexp))
      ;; convert a s-exp into a package record
      (let ((url (get-string-by-type package-sexp 'url ""))
            (name (get-list-by-type package-sexp 'name '()))
            (version (get-string-by-type package-sexp 'version "1.0"))
            (size (get-number-by-type package-sexp 'size 'unset))
            (checksum (get-list-by-type package-sexp 'checksum 'unset))
            (library-sexps (get-children-by-type package-sexp 'library)))
        (let* ((libraries (map library-from-sexp library-sexps))
               (package (make-snow2-package
                         name version (uri-reference url) libraries #f
                         size checksum #f)))
          ;; backlink to packages
          (for-each
           (lambda (library)
             (set-snow2-library-package! library package))
           libraries)
          package)))


    (define (package->sexp package)
      (snow-assert (snow2-package? package))
      `(package
        (name ,(snow2-package-name package))
        (version ,(snow2-package-version package))
        (url ,(if (snow2-package-url package)
                  (uri->string (snow2-package-url package))
                  "")) ;; XXX why ""?  why not #f
        ,@(let ((size (snow2-package-size package)))
            (if (not (eq? size 'unset)) `((size ,size)) '()))
        ,@(let ((checksum (snow2-package-checksum package)))
            (if (not (eq? checksum 'unset)) `((checksum ,checksum)) '()))
        ,@(map library->sexp (snow2-package-libraries package))))


    (define (package->html package)
      (snow-assert (snow2-package? package))
      `(html:tr
        (@ (class "package-tr"))
        (html:td
         (@ (class "package-td"))
         (html:a
          (@ (class "package-a")
             (href ,(uri->string (snow2-package-url package))))
          ,(snow2-package-get-readable-name package)))
        (html:td
         (@ (class "library-td"))
         (html:ul
          ,@(map library->html (snow2-package-libraries package))))))


    (define (package-from-metafile-name package-filename)
      (snow-assert (string? package-filename))
      (cond ((not (file-exists? package-filename))
             (display "can't read package meta-file: ")
             (display package-filename)
             (newline)
             (exit 1))
            (else
             (let* ((package-port (open-input-file package-filename))
                    (package-sexp (read package-port))
                    (package
                     (cond ((eof-object? package-sexp)
                            (display "package meta-file is empty: ")
                            (display package-filename)
                            (newline)
                            (exit 1))
                           (else
                            (package-from-sexp package-sexp)))))
               (close-input-port package-port)

               (cond ((not package)
                      (display "can't read package metafile: ")
                      (display package-filename)
                      (exit 1)))

               package))))


    (define not-hyphen-char-set
      (char-set-complement (list->char-set (list #\-))))

    (define (name-sorter name-a name-b)
      (snow-assert (string? name-a))
      (snow-assert (string? name-b))
      ;; sort packages by their names.  sort by hyphen-delimited sections
      ;; and if a section is numeric, sort numerically rather than
      ;; alpha-numerically.  This is so srfi-1-lists will be before
      ;; srfi-106-basic-socket-interface, etc.
      (let* ((name-a-parts (string-tokenize name-a not-hyphen-char-set))
             (name-b-parts (string-tokenize name-b not-hyphen-char-set)))
        (let loop ((name-a-parts name-a-parts)
                   (name-b-parts name-b-parts))
          (cond ((null? name-a-parts) #t)
                ((null? name-b-parts) #f)
                (else
                 (let* ((a-part (car name-a-parts))
                        (a-as-num (string->number a-part))
                        (b-part (car name-b-parts))
                        (b-as-num (string->number b-part)))
                   (cond ((and a-as-num b-as-num)
                          (< a-as-num b-as-num))
                         ((string-ci<? a-part b-part) #t)
                         (else
                          (loop (cdr name-a-parts)
                                (cdr name-b-parts))))))))))


    (define (repository-from-sexp repository-sexp)
      (snow-assert (list? repository-sexp))
      ;; convert an s-exp into a repository record
      (cond ((not (list? repository-sexp))
             (error "repository definition isn't a list." repository-sexp))
            ((null? repository-sexp)
             (error "repository s-exp is empty."))
            ((not (eq? (car repository-sexp) 'repository))
             (error "this doesn't look like a repository."))
            (else
             (let* ((package-sexps
                     (get-children-by-type repository-sexp 'package))
                    (packages (map package-from-sexp package-sexps))
                    (sibling-sexps
                     (get-children-by-type repository-sexp 'sibling))
                    (siblings (map sibling-from-sexp sibling-sexps))
                    (url (get-string-by-type repository-sexp 'url #f))
                    (name (get-string-by-type repository-sexp 'name #f))
                    (repo (make-snow2-repository
                           name siblings packages #f
                           (if url (uri-reference url) #f)
                           #f)))
               ;; backlink package to repository
               (for-each
                (lambda (package)
                  (set-snow2-package-repository! package repo))
                packages)
               repo))))


    (define (repository->sexp repository)
      (snow-assert (snow2-repository? repository))
      `(repository
        ,@(if (snow2-repository-url repository)
              `((url ,(uri->string (snow2-repository-url repository))))
              '())
        ,@(if (snow2-repository-name repository)
              `((name ,(snow2-repository-name repository)))
              '())
        ,@(map sibling->sexp (snow2-repository-siblings repository))
        ,@(map package->sexp (snow2-repository-packages repository))
        ))

    (define (repository->html repository)
      (snow-assert (snow2-repository? repository))
      (serialize-sxml
       `(*TOP* (@ (*NAMESPACES* (html "http://www.w3.org/1999/xhtml")))
               (meta (@ (http-equiv "Content-Type")
                        (content "text/html; charset=UTF-8")))
               (html:html
                (html:head (html:link (@ (rel "stylesheet")
                                         (type "text/css")
                                         (href "index.css"))))
                (html:body
                 (html:h1
                  ,(if (and
                        (snow2-repository-name repository)
                        (not (equal? "" (snow2-repository-name repository))))
                       (snow2-repository-name repository)
                       "Snow2 Repository"))

                 ,@(if (pair? (snow2-repository-siblings repository))
                       (list
                        '(html:h2 "Siblings")
                        (map sibling->html
                             (snow2-repository-siblings repository)))
                       '())

                 ;; (html:h2 "Packages")
                 (html:table
                  (html:thead
                   (html:tr (html:th "Packages") (html:th "Libraries")))
                  (html:tbody
                   ,@(map package->html
                          (sort
                           (snow2-repository-packages repository)
                           (lambda (a b)
                             (name-sorter
                              (snow2-package-get-readable-name a)
                              (snow2-package-get-readable-name b))))))))))
       'ns-prefixes '((*default* . "http://www.w3.org/1999/xhtml"))))


    (define (read-repository in-port)
      ;; read an s-exp from in-port and convert it to a repository record.
      (snow-assert (input-port? in-port))
      (let* ((repository-sexp
              (read
               (binary-port->textual-port in-port))))
        (repository-from-sexp repository-sexp)))


    (define (get-snow2-repo-name package)
      (snow-assert (snow2-package? package))
      (cond ((and (list? (snow2-package-name package))
                  (not (null? (snow2-package-name package))))
             (snow2-package-name package))
            ((not (null? snow2-package-libraries))
             (let* ((lib (car (snow2-package-libraries package)))
                    (lib-name (snow2-library-name lib)))
               (cond ((and (list? lib-name)
                           (not (null? lib-name)))
                      (symbol->string (car lib-name)))
                     (else '()))))
            (else
             '())))


    (define (snow2-packages-libraries packages)
      ;; return a flattened list of all libraries in the given packages
      (snow-assert (list? packages))
      (fold
       (lambda (package lst)
         (append (snow2-package-libraries package) lst))
       '()
       packages))


    (define (uri->hashtable-key uri)
      (snow-assert (uri-reference? uri))
      (uri->string uri))


    (define (package-contains-library? package library-name)
      ;; return #t if a package contains any libraries with the given name
      (snow-assert (snow2-package? package))
      (snow-assert (list? library-name))
      (let loop ((libraries (snow2-package-libraries package)))
        (cond ((null? libraries) #f)
              (else
               (let ((library (car libraries)))
                 ;; (write (snow2-library-name library))
                 ;; (display " VS ")
                 ;; (write library-name)
                 ;; (newline)
                 (if (equal? (snow2-library-name library) library-name)
                     #t
                     (loop (cdr libraries))))))))


    (define (find-packages-with-library library-name)
      ;; find all packages with a library of the given name
      (snow-assert (list? library-name))
      (let ((iter (make-repository-iterator)))
        (let r-loop ((repository (get-next-repository iter))
                     (candidate-packages '()))
          (cond
           ((not repository)
            (cond ((null? candidate-packages)
                   ;; (display "couldn't find library: " (current-error-port))
                   ;; (write library-name (current-error-port))
                   ;; (newline (current-error-port))
                   ;; (flush-output-port (current-error-port))
                   (add-to-unfound-libs library-name)
                   '())
                  ;; XXX rather than just taking the last one,
                  ;; select one based on version requirements, etc
                  (else candidate-packages)))
           (else
            (let loop ((packages (snow2-repository-packages repository))
                       (candidate-packages candidate-packages))
              (cond ((null? packages)
                     (r-loop (get-next-repository iter) candidate-packages))
                    (else
                     (let ((package (car packages)))
                       (if (package-contains-library? package library-name)
                           (loop (cdr packages)
                                 (cons package candidate-packages))
                           (loop (cdr packages)
                                 candidate-packages)))))))))))


    (define (find-package-with-library library-name)
      ;; find the last package that contains a library with the given name
      (snow-assert (list? library-name))
      (let ((pwl (find-packages-with-library library-name)))
        (if (pair? pwl) (car pwl) #f)))


    (define (find-packages-with-libraries library-names)
      ;; return a list of packages that contain any libraries
      ;; with the given library-names.
      (snow-assert (list? library-names))
      (let ((package-url-ht (make-hash-table)))
        (for-each
         (lambda (library-name)
           (let ((package
                  (find-package-with-library library-name)))
             (cond (package
                    (hash-table-set!
                     package-url-ht
                     (uri->hashtable-key (snow2-package-absolute-url package))
                     package)))))
         library-names)
        (hash-table-values package-url-ht)))


    (define (library-is-used-for-steps library steps)
      (snow-assert (snow2-library? library))
      (snow-assert (list? steps))
      (pair? (lset-intersection eq? steps (snow2-library-use-for library))))


    (define (find-libraries-for-steps package steps)
      ;; search package for libraries that are used-for the given step
      (snow-assert (snow2-package? package))
      (snow-assert (list? steps))
      (let loop ((libraries (snow2-package-libraries package))
                 (result '()))
        (cond ((null? libraries) result)
              ((library-is-used-for-steps (car libraries) steps)
               (loop (cdr libraries) (cons (car libraries) result)))
              (else (loop (cdr libraries) result)))))


    (define (library-from-name library-name)
      ;; search repositories for a library record with the given name.
      ;; return the first matching record or #f.
      (snow-assert (list? library-name))
      (let* ((package (find-package-with-library library-name)))
        (cond (package
               (let loop ((libraries (snow2-package-libraries package)))
                 (cond ((null? libraries) #f)
                       ((equal? library-name
                                (snow2-library-name (car libraries)))
                        (car libraries))
                       (else (loop (cdr libraries)))))))))


    (define (gather-depends libraries steps)
      ;; returns a list of snow2-packages
      (snow-assert (list? libraries))
      (snow-assert (list? steps))
      (let ((lib-name-ht (make-hash-table))
            (package-url-ht (make-hash-table)))
        (for-each
         (lambda (library)
           (snow-assert (snow2-library? library))
           ;; only chase libraries that are used for the indicated
           ;; steps (usually 'test and/or 'final)
           (if (library-is-used-for-steps library steps)

               (let ((lib-name (snow2-library-name library)))
                 (let ((package (find-package-with-library lib-name)))
                   (cond (package
                          (hash-table-set! lib-name-ht lib-name library)
                          (hash-table-set!
                           package-url-ht
                           (uri->hashtable-key
                            (snow2-package-absolute-url package))
                           package))))

                 (for-each
                  (lambda (depend)
                    (let ((package (find-package-with-library depend)))
                      (cond (package
                             (let ((libs (snow2-package-libraries package)))
                               (hash-table-set!
                                package-url-ht
                                (uri->hashtable-key
                                 (snow2-package-absolute-url package))
                                package)
                               ;; XXX if the same lib is in more than one
                               ;; package, there should be some reason to
                               ;; pick one over the other?
                               (for-each
                                (lambda (lib)
                                  (hash-table-set!
                                   lib-name-ht (snow2-library-name lib) lib))
                                libs))))))
                  (snow2-library-depends library)))))
         libraries)

        (if (= (length (hash-table-keys lib-name-ht)) (length libraries))
            ;; nothing new added this pass, so we've finished.
            (hash-table-values package-url-ht)
            ;; we found more, go around again.
            (gather-depends (hash-table-values lib-name-ht) steps))))


    (define (caching-get-repository repository-url)
      (snow-assert (uri-reference? repository-url))
      (cond ((url-in-repository-hash? repository-url)
             (get-repository-by-url repository-url))
            (else
             (let ((new-repo (get-repository repository-url)))
               (cond ((repository-in-hash? new-repo)
                      ;; if repository-url is a local filesystem path,
                      ;; get-repository may have called get-siblings, which may
                      ;; have caused a remote version of this repository to be
                      ;; placed in the repository-hash.  If this has happened,
                      ;; replace the remote version of the repository with the
                      ;; local one.
                      (let ((already-in-table
                             (hash-table-ref
                              repository-hash (repository-hash-key new-repo))))
                        (if (and (not (snow2-repository-local already-in-table))
                                 (snow2-repository-local new-repo))
                            (replace-repository-in-hash
                             already-in-table new-repo))))
                     (else
                      ;; put this repository into repository-hash for the
                      ;; first time.
                      (add-repository-to-hash repository-url new-repo)))))))


    (define (get-siblings repository)
      (snow-assert (snow2-repository? repository))
      (for-each
       caching-get-repository
       (map snow2-sibling-url (snow2-repository-siblings repository))))


    (define (get-repository repository-url . maybe-error-on-bad-repo)
      ;; read index.scm from over http(s) or from a local filesystem.
      ;; if from a local directory, make sure the repository looks sane:
      ;; it must have a "packages" subdirectory and an index.scm file.
      ;; if it's missing either of these, raise an error unless
      ;; maybe-error-on-bad-repo is #f.
      (snow-assert (uri-reference? repository-url))
      (cond ((memq (uri-scheme repository-url) '(http https))
             (delay
               (begin

                 ;; get repository over http

                 (snow2-trace
                  `(downloading index ,(uri->string repository-url)))

                 (guard
                     (err (#t
                           (display
                            "Warning: unable to fetch repository index: ")
                           (write (uri->string repository-url))
                           (newline)
                           (write (error-object-message err))
                           (newline)
                           (write (error-object-irritants err))
                           (newline)
                           #f))
                   (let ((repository
                          (http-call-with-request-body
                           (uri->string repository-url)
                           read-repository)))
                     (set-snow2-repository-local! repository #f)
                     (set-snow2-repository-url! repository repository-url)
                     (get-siblings repository)
                     repository)))))
            (else
             ;; read from local filesystem repository
             (snow2-trace `(loading index ,(uri->string repository-url)))
             (let* ((error-on-bad-repo (if (pair? maybe-error-on-bad-repo)
                                           (car maybe-error-on-bad-repo)
                                           #t))
                    (repo-dirname (uri->string repository-url))
                    (packages-dirname
                     (snow-make-filename repo-dirname "packages"))
                    (index-filename
                     (snow-make-filename repo-dirname "index.scm"))
                    )
               (define (bad-local-repo why)
                 (cond (error-on-bad-repo
                        (display "local repository ")
                        (write repo-dirname)
                        (display " is incomplete: ")
                        (display why)
                        (newline)
                        (exit 1))
                       (else #f)))
               (cond
                ((or (not (file-exists? packages-dirname))
                     (not (snow-file-directory? packages-dirname)))
                 (bad-local-repo "missing packages subdirectory"))
                ((not (file-exists? index-filename))
                 (let* ((repository (repository-from-sexp '(repository))))
                   (set-snow2-repository-local! repository repository-url)
                   repository))
                (else
                 (let* ((in-port (open-binary-input-file index-filename))
                        (repository (read-repository in-port)))
                   (set-snow2-repository-local! repository repository-url)

                   ;; XXX
                   ;; should the repository s-expression have name
                   ;; and url fields?
                   ;; (set-snow2-repository-url! repository repository-url)

                   (close-input-port in-port)
                   (get-siblings repository)
                   repository)))))))


    (define (merge-packages! dst-package src-package verbose)
      (snow-assert (snow2-package? dst-package))
      (snow-assert (snow2-package? src-package))
      (snow-assert (boolean? verbose))
      ;; update dst-package's list of libraries
      (set-snow2-package-libraries!
       dst-package
       (map copy-snow2-library (snow2-package-libraries src-package)))
      ;; set library backpointers to package
      (for-each
       (lambda (library)
         (set-snow2-library-package! library dst-package))
       (snow2-package-libraries dst-package))
      (set-snow2-package-dirty! dst-package #t))


    (define (refresh-package-from-filename repository package-filename verbose)
      ;; read a file that contains a package s-exp and update the copy
      ;; in repository.  return the updated package.
      (snow-assert (snow2-repository? repository))
      (snow-assert (string? package-filename))
      (snow-assert (boolean? verbose))
      (let ((updated-package (package-from-metafile-name package-filename)))

        (define (package-name/url-matches-updated? repo-package)
          ;; return true if repo-package has the same name and url as
          ;; updated-package
          (and (equal? (snow2-package-name repo-package)
                       (snow2-package-name updated-package))
               (uri-equal?
                (snow2-package-absolute-url repo-package)
                (snow2-package-absolute-url updated-package))))

        (set-snow2-package-repository! updated-package repository)

        (let ((repo-package (find package-name/url-matches-updated?
                                  (snow2-repository-packages repository))))
          (cond ((not repo-package)
                 ;; we found a package file, but it's not in the repository's
                 ;; index.scm file.  just add it to the list.
                 (set-snow2-repository-packages!
                  repository
                  (cons updated-package
                        (snow2-repository-packages repository)))
                 (set-snow2-repository-dirty! repository #t)
                 updated-package)
                (else
                 ;; we found the package to update
                 (cond ((not (snow2-packages-equal?
                              repo-package updated-package))
                        ;; merge the fields in updated-package
                        ;; into repo-package
                        (merge-packages! repo-package
                                         updated-package
                                         verbose)
                        (set-snow2-repository-dirty! repository #t)))
                 repo-package)))))



    (define (find-libraries-by-name container library-name)
      ;; returns a (possibly empty) list of library structs that
      ;; have the given name and are somewhere inside container.
      ;; container can be any of:
      ;;   snow2-library, snow2-package, snow2-repository
      ;; or a list containing these.
      (snow-assert (or (snow2-library? container)
                       (snow2-package? container)
                       (snow2-repository? container)
                       (list? container)))
      (snow-assert (list? library-name))
      (cond ((snow2-library? container)
             (cond ((equal? library-name (snow2-library-name container))
                    (list container))
                   (else '())))
            (else
             (fold append '()
                   (map (lambda (child)
                          (find-libraries-by-name child library-name))
                        (cond ((snow2-package? container)
                               (snow2-package-libraries container))
                              ((snow2-repository? container)
                               (snow2-repository-packages container))
                              ((list? container) container)
                              (else
                               (error "unknown snow2 container type"
                                      container))))))))


    (define (lib-sexp->name lib-sexp)
      ;; lib-sexp is a (read ...) of a library .sld file.  return
      ;; whatever is after define-library.
      (snow-assert (list? lib-sexp))
      (let ((name (cadr lib-sexp)))
        (cond ((list? name) name)
              ((symbol? name)
               ;; name is in dotted format.
               (string-tokenize (symbol->string name)
                                (string->char-set ".")))
              (else
               ;; name is in dotted format.
               (string-tokenize name (string->char-set "."))))))


    (define (local-repository->in-fs-index-path local-repository)
      ;; given an on-disk repository, return a path to index.scm
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.scm"))))

    (define (local-repository->in-fs-index-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.scm
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (snow-combine-filename-parts
       (local-repository->in-fs-index-path local-repository)))


    (define (local-repository->in-fs-html-path local-repository)
      ;; given an on-disk repository, return a path to index.html
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.html"))))

    (define (local-repository->in-fs-html-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.html
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (snow-combine-filename-parts
       (local-repository->in-fs-html-path local-repository)))


    (define (local-repository->in-fs-css-path local-repository)
      ;; given an on-disk repository, return a path to index.css
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.css"))))

    (define (local-repository->in-fs-css-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.css
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-repository-local local-repository))
      (snow-combine-filename-parts
       (local-repository->in-fs-css-path local-repository)))



    (define (local-repository->in-fs-tgz-path local-repository package)
      ;; within a local repository, return a path on the filesystem to
      ;; a tgz for the given package
      ;; XXX this isn't right
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-package? package))
      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             (url (snow2-package-absolute-path package)))
        (reverse (cons (last (uri-path url)) (reverse repo-path)))))

    (define (local-repository->in-fs-tgz-filename local-repository package)
      ;; within a local repository, return a path/filename on the filesystem
      ;; to a tgz for the given package
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-package? package))
      (snow-combine-filename-parts
       (local-repository->in-fs-tgz-path local-repository package)))


    (define (local-repository->in-fs-lib-path local-repository lib)
      ;; return path to library source file within a local repository
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-library? lib))
      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             (in-pkg-lib-path (snow-split-filename (snow2-library-path lib))))
        (append repo-path in-pkg-lib-path)))

    (define (local-repository->in-fs-lib-filename local-repository lib)
      ;; return filename of library source file within a local repository
      (snow-assert (snow2-repository? local-repository))
      (snow-assert (snow2-library? lib))
      (snow-combine-filename-parts
       (local-repository->in-fs-lib-path local-repository lib)))


    (define (repo-path->file-path repo-path file-path)
      ;;; repo-path might be .../index.scm or just .../
      (snow-assert (list? repo-path))
      (snow-assert (list? file-path))
      (cond ((equal? (last repo-path) "index.scm")
             (append (reverse (cdr (reverse repo-path))) file-path))
            (else
             (append repo-path file-path))))

    (define (repo-url->file-url repo-url file-path)
      (snow-assert (uri-reference? repo-url))
      (snow-assert (list? file-path))
      (cond ((not repo-url) #f)
            (else
             (update-uri repo-url 'path
                         (repo-path->file-path
                          (uri-path repo-url)
                          file-path)))))

    (define (sanity-check-repository repository)
      (snow-assert (snow2-repository? repository))
      (for-each
       (lambda (package)
         (cond ((eq? (snow2-repository-force (snow2-package-repository package))
                     (snow2-repository-force repository))
                (sanity-check-package package))
               (else
                (error
                 (display "sanity check failed for repository: ")
                 (write (snow2-repository-url repository))
                 (newline)
                 (display "package ")
                 (write (snow2-package-url package))
                 (display " doesn't backlink.\n")
                 (error "sanity check failed")
                 (exit 1))))
         )
       (snow2-repository-packages repository)))


    (define (sanity-check-package package)
      (snow-assert (snow2-package? package))
      (for-each
       (lambda (library)
         (cond ((eq? (snow2-library-package library) package)
                ;; (sanity-check-library library)
                #t
                )
               (else
                (error
                 (display "sanity check failed for package: ")
                 (write (uri->string (snow2-package-url package)))
                 (newline)
                 (display "library ")
                 (write (snow2-library-path library))
                 (display " doesn't backlink.\n")
                 (error "sanity check failed")
                 (exit 1)))))
       (snow2-package-libraries package)))


    ))

