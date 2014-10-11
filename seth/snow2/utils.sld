(define-library (seth snow2 utils)
  (export get-repository
          read-repository
          get-repositories-and-siblings
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

          sanity-check-repository
          sanity-check-package

          repository->html
          )

  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context))
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any fold
                        lset-intersection last drop-right)
                  (only (chibi) read)))
   (else (import (scheme read)
                 (srfi 1))))
  (cond-expand
   (chibi (import (chibi filesystem)))
   (else))
  (import (snow extio)
          (srfi 13)
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


    (define (snow2-package-absolute-url/path repo-url package)
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
      ;; the url for a package may be relative to the repository's url.
      ;; this will return an absolute version.
      (let ((repository (snow2-package-repository package)))
        (cond ((not (snow2-repository-url repository)) #f)
              (else
               (snow2-package-absolute-url/path
                (snow2-repository-url repository) package)))))


    (define (snow2-package-absolute-path package)
      ;; combine a local repsitory's in-filesystem path
      ;; with the package url
      (let ((repository (snow2-package-repository package)))
        (cond ((not (snow2-repository-local repository)) #f)
              (else
               (snow2-package-absolute-url/path
                (snow2-repository-local repository) package)))))


    (define (depend-from-sexp depend-sexp)
      ;; depend-sexp will be a library name, like (snow hello)
      depend-sexp)


    (define (depend->sexp depend)
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
      (let ((name (get-string-by-type sibling-sexp 'name #f))
            (url (get-string-by-type sibling-sexp 'url #f))
            (trust (get-number-by-type sibling-sexp 'trust 0.5)))
        (make-snow2-sibling name (uri-reference url) trust)))

    (define (sibling->sexp sibling)
      `(sibling
        (name ,(snow2-sibling-name sibling))
        (url ,(uri->string (snow2-sibling-url sibling)))
        (trust ,(snow2-sibling-trust sibling))))


    (define (sibling->html sibling)
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
      ;; convert an s-exp into a library record
      (let ((name (get-list-by-type library-sexp 'name '()))
            (path (get-string-by-type library-sexp 'path #f))
            (depends-sexps (get-multi-args-by-type library-sexp 'depends '()))
            (version (get-string-by-type library-sexp 'version "1.0"))
            (homepage (get-args-by-type library-sexp 'homepage '()))
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
                maintainers
                authors
                description
                license
                #f
                use-for)))))


    (define (copy-snow2-library library)
      (make-snow2-library
       (deep-copy (snow2-library-name library))
       (deep-copy (snow2-library-path library))
       (deep-copy (snow2-library-depends library))
       (deep-copy (snow2-library-version library))
       (deep-copy (snow2-library-homepage library))
       (deep-copy (snow2-library-maintainers library))
       (deep-copy (snow2-library-authors library))
       (deep-copy (snow2-library-description library))
       (deep-copy (snow2-library-license library))
       (snow2-library-package library)
       (deep-copy (snow2-library-use-for library))))


    (define (library->sexp library)
      `(library
        (name ,(snow2-library-name library))
        (path ,(snow2-library-path library))
        (version ,(snow2-library-version library))
        (homepage ,@(snow2-library-homepage library))
        (maintainers ,@(snow2-library-maintainers library))
        (authors ,@(snow2-library-authors library))
        (description ,@(snow2-library-description library))
        (license ,@(snow2-library-license library))
        (depends ,@(map depend->sexp (snow2-library-depends library)))
        (use-for ,@(snow2-library-use-for library))
        ))


    (define (library->html library)
      (let ((name (snow2-library-name library))
            (path (snow2-library-path library))
            (version (snow2-library-version library))
            (homepage (cond ((pair? (snow2-library-homepage library))
                             (car (snow2-library-homepage library)))
                            (else #f)))
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
               package))))


    (define (repository-from-sexp repository-sexp)
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
      (serialize-sxml
       `(*TOP* (@ (*NAMESPACES* (html "http://www.w3.org/1999/xhtml")))
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

                 (html:h2 "Siblings")
                 ,@(map sibling->html (snow2-repository-siblings repository))

                 (html:h2 "Packages")
                 (html:table
                  (html:thead
                   (html:tr (html:th "Packages") (html:th "Libraries")))
                  (html:tbody
                   ,@(map package->html (snow2-repository-packages repository))
                   )))))
       'ns-prefixes '((*default* . "http://www.w3.org/1999/xhtml"))))


    (define (read-repository in-port)
      ;; read an s-exp from in-port and convert it to a repository record.
      (let* ((repository-sexp
              (read
               (binary-port->textual-port in-port))))
        (repository-from-sexp repository-sexp)))


    (define (get-snow2-repo-name package)
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
      (fold
       (lambda (package lst)
         (append (snow2-package-libraries package) lst))
       '()
       packages))


    (define uri->hashtable-key uri->string)

    (define (package-contains-library? package library-name)
      ;; return #t if a package contains any libraries with the given name
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


    (define (find-packages-with-library repositories library-name)
      ;; find all packages with a library of the given name
      (let r-loop ((repositories repositories)
                   (candidate-packages '()))
        (cond
         ((null? repositories)
          (cond ((null? candidate-packages)
                 ;; (error "couldn't find library" library-name)
                 (display "couldn't find library: " (current-error-port))
                 (write library-name (current-error-port))
                 (newline (current-error-port))
                 (flush-output-port (current-error-port))
                 '())
                ;; XXX rather than just taking the last one,
                ;; select one based on version requirements, etc
                (else candidate-packages)))
         (else
          (let loop ((packages (snow2-repository-packages (car repositories)))
                     (candidate-packages candidate-packages))
            (cond ((null? packages)
                   (r-loop (cdr repositories)
                           candidate-packages))
                  (else
                   (let ((package (car packages)))
                     (if (package-contains-library? package library-name)
                         (loop (cdr packages)
                               (cons package candidate-packages))
                         (loop (cdr packages)
                               candidate-packages))))))))))


    (define (find-package-with-library repositories library-name)
      ;; find the last package that contains a library with the given name
      (let ((pwl (find-packages-with-library repositories library-name)))
        (if (pair? pwl) (car pwl) #f)))


    (define (find-packages-with-libraries repositories library-names)
      ;; return a list of packages that contain any libraries
      ;; with the given library-names.
      (let ((package-url-ht (make-hash-table)))
        (for-each
         (lambda (library-name)
           (let ((package
                  (find-package-with-library repositories library-name)))
             (cond (package
                    (hash-table-set!
                     package-url-ht
                     (uri->hashtable-key (snow2-package-absolute-url package))
                     package)))))
         library-names)
        (hash-table-values package-url-ht)))


    (define (library-is-used-for-steps library steps)
      (pair? (lset-intersection eq? steps (snow2-library-use-for library))))


    (define (find-libraries-for-steps package steps)
      ;; search package for libraries that are used-for the given step
      (let loop ((libraries (snow2-package-libraries package))
                 (result '()))
        (cond ((null? libraries) result)
              ((library-is-used-for-steps (car libraries) steps)
               (loop (cdr libraries) (cons (car libraries) result)))
              (else (loop (cdr libraries) result)))))


    (define (library-from-name repositories library-name)
      ;; search repositories for a library record with the given name.
      ;; return the first matching record or #f.
      (let* ((package (find-package-with-library repositories library-name)))
        (cond (package
               (let loop ((libraries (snow2-package-libraries package)))
                 (cond ((null? libraries) #f)
                       ((equal? library-name
                                (snow2-library-name (car libraries)))
                        (car libraries))
                       (else (loop (cdr libraries)))))))))


    (define (gather-depends repositories libraries steps)
      ;;
      ;; returns a list of snow2-packages
      ;;
      (let ((lib-name-ht (make-hash-table))
            (package-url-ht (make-hash-table)))
        (for-each
         (lambda (library)

           ;; only chase libraries that are used for the indicated
           ;; steps (usually 'test and/or 'final)
           (if (library-is-used-for-steps library steps)

               (let ((lib-name (snow2-library-name library)))
                 (let ((package
                        (find-package-with-library repositories lib-name)))
                   (cond (package
                          (hash-table-set! lib-name-ht lib-name library)
                          (hash-table-set!
                           package-url-ht
                           (uri->hashtable-key
                            (snow2-package-absolute-url package))
                           package))))

                 (for-each
                  (lambda (depend)
                    (let ((package
                           (find-package-with-library repositories depend)))
                      (cond (package
                             (let ((libs (snow2-package-libraries package)))
                               (hash-table-set!
                                package-url-ht
                                (uri->hashtable-key
                                 (snow2-package-absolute-url package))
                                package)
                               ;; XXX if the same lib is in more than one
                               ;; package, there should be some reason to pick one
                               ;; over the other?
                               (for-each
                                (lambda (lib)
                                  (hash-table-set! lib-name-ht
                                                   (snow2-library-name lib) lib))
                                libs))))))
                  (snow2-library-depends library)))))
         libraries)

        (if (= (length (hash-table-keys lib-name-ht)) (length libraries))
            ;; nothing new added this pass, so we've finished.
            (hash-table-values package-url-ht)
            ;; we found more, go around again.
            (gather-depends
             repositories (hash-table-values lib-name-ht) steps))))


    (define (get-repository repository-url . maybe-error-on-bad-repo)
      ;; read index.scm from over http(s) or from a local filesystem.
      ;; if from a local directory, make sure the repository looks sane:
      ;; it must have a "packages" subdirectory and an index.scm file.
      ;; if it's missing either of these, raise an error unless
      ;; maybe-error-on-bad-repo is #f.
      (cond ((memq (uri-scheme repository-url) '(http https))
             ;; get repository over http
             (guard
              (err (#t
                    (display "Warning: unable to fetch repository index: ")
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
                repository)))
            (else
             ;; read from local filesystem repository
             (let* ((error-on-bad-repo (if (pair? maybe-error-on-bad-repo)
                                           (car maybe-error-on-bad-repo)
                                           #t))
                    (repo-dirname (uri->string repository-url))
                    ;; (tests-dirname (snow-make-filename repo-dirname "tests"))
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
               (cond ;; ((or (not (file-exists? tests-dirname))
                     ;;      (not (snow-file-directory? tests-dirname)))
                     ;;  (bad-local-repo "missing tests subdirectory"))
                     ((or (not (file-exists? packages-dirname))
                          (not (snow-file-directory? packages-dirname)))
                      (bad-local-repo "missing packages subdirectory"))
                     ((not (file-exists? index-filename))
                      (let* ((repository (repository-from-sexp '(repository))))
                        (set-snow2-repository-local! repository repository-url)
                        ;; (set-snow2-repository-url! repository repository-url)
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
                        repository)))))))


    (define (get-repositories-and-siblings repositories repository-urls)
      (define (make-repo-has-url? url)
        (lambda (repository)
          (and (snow2-repository-url repository)
               (uri-equal? (snow2-repository-url repository) url))))
      (define (get-sibling-urls repository)
        (map snow2-sibling-url (snow2-repository-siblings repository)))
      (cond ((null? repository-urls) repositories)
            (else
             (let ((repository-url (car repository-urls)))
               (cond ((any (make-repo-has-url? repository-url) repositories)
                      ;; we've already loaded this one.
                      (get-repositories-and-siblings
                       repositories (cdr repository-urls)))
                     (else
                      ;; this was previously unloaded
                      (let ((repository (get-repository repository-url)))
                        (if repository
                            (let ((sibling-urls (get-sibling-urls repository)))
                              (get-repositories-and-siblings
                               (cons repository repositories)
                               (append (cdr repository-urls) sibling-urls)))
                            ;; perhaps this should be a fatal error.
                            ;; for now, just try to continue.
                            (get-repositories-and-siblings
                             repositories (cdr repository-urls))))))))))


    (define (merge-packages! dst-package src-package verbose)
      ;; (cond (verbose
      ;;        (display "package file changed.\n")
      ;;        (write (package->sexp dst-package))
      ;;        (newline)
      ;;        (write (package->sexp src-package))
      ;;        (newline)))

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
      (let ((updated-package (package-from-metafile-name package-filename)))
        (cond ((not updated-package)
               (error "can't read package metafile." package-filename)))

        (set-snow2-package-repository! updated-package repository)

        (let loop ((repo-packages (snow2-repository-packages repository)))
          (cond ((null? repo-packages)
                 ;; we found a package file, but it's not in the repository's
                 ;; index.scm file.  just add it to the list.
                 (set-snow2-repository-packages!
                  repository
                  (cons updated-package
                        (snow2-repository-packages repository)))
                 (set-snow2-package-repository! updated-package repository)
                 (set-snow2-repository-dirty! repository #t)
                 (set-snow2-package-dirty! updated-package #t)
                 updated-package)
                (else
                 (let ((repo-package (car repo-packages)))
                   (cond ((and (equal? (snow2-package-name repo-package)
                                       (snow2-package-name updated-package))
                               (uri-equal?
                                (snow2-package-absolute-url repo-package)
                                (snow2-package-absolute-url updated-package)))
                          ;; we found the package to update
                          (cond ((not (snow2-packages-equal?
                                       repo-package updated-package))
                                 ;; merge the fields in updated-package
                                 ;; into repo-package
                                 (merge-packages! repo-package
                                                  updated-package
                                                  verbose)
                                 (set-snow2-repository-dirty! repository #t)))
                          repo-package)
                         (else
                          ;; this package didn't match the one we read
                          ;; from package-filename, keep searching.
                          (loop (cdr repo-packages))))))))))



    (define (find-libraries-by-name container library-name)
      ;; returns a (possibly empty) list of library structs that
      ;; have the given name and are somewhere inside container.
      ;; container can be any of:
      ;;   snow2-library, snow2-package, snow2-repository
      ;; or a list containing these.
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
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.scm"))))

    (define (local-repository->in-fs-index-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.scm
      (snow-combine-filename-parts
       (local-repository->in-fs-index-path local-repository)))


    (define (local-repository->in-fs-html-path local-repository)
      ;; given an on-disk repository, return a path to index.html
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.html"))))

    (define (local-repository->in-fs-html-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.html
      (snow-combine-filename-parts
       (local-repository->in-fs-html-path local-repository)))


    (define (local-repository->in-fs-css-path local-repository)
      ;; given an on-disk repository, return a path to index.css
      (let* ((repo-path (uri-path (snow2-repository-local local-repository))))
        (append repo-path (list "index.css"))))

    (define (local-repository->in-fs-css-filename local-repository)
      ;; given an on-disk repository, return the (perhaps relative)
      ;; path and filename of index.css
      (snow-combine-filename-parts
       (local-repository->in-fs-css-path local-repository)))




    (define (local-repository->in-fs-tgz-path local-repository package)
      ;; within a local repository, return a path on the filesystem to
      ;; a tgz for the given package
      ;; XXX this isn't right
      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             (url (snow2-package-absolute-path package)))
        (reverse (cons (last (uri-path url)) (reverse repo-path)))))

    (define (local-repository->in-fs-tgz-filename local-repository package)
      ;; within a local repository, return a path/filename on the filesystem
      ;; to a tgz for the given package
      (snow-combine-filename-parts
       (local-repository->in-fs-tgz-path local-repository package)))


    (define (local-repository->in-fs-lib-path local-repository lib)
      ;; return path to library source file within a local repository
      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             (in-pkg-lib-path (snow-split-filename (snow2-library-path lib))))
        (append repo-path in-pkg-lib-path)))

    (define (local-repository->in-fs-lib-filename local-repository lib)
      ;; return filename of library source file within a local repository
      (snow-combine-filename-parts
       (local-repository->in-fs-lib-path local-repository lib)))


    (define (repo-path->file-path repo-path file-path)
      ;;; repo-path might be .../index.scm or just .../
      (cond ((equal? (last repo-path) "index.scm")
             (append (reverse (cdr (reverse repo-path))) file-path))
            (else
             (append repo-path file-path))))

    (define (repo-url->file-url repo-url file-path)
      (cond ((not repo-url) #f)
            (else
             (update-uri repo-url 'path
                         (repo-path->file-path
                          (uri-path repo-url)
                          file-path)))))

    (define (sanity-check-repository repository)
      (for-each
       (lambda (package)
         (cond ((eq? (snow2-package-repository package) repository)
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
                 (exit 1)))))
       (snow2-repository-packages repository)))


    (define (sanity-check-package package)
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

