(define-library (seth snow2-utils)
  (export get-repository
          read-repository
          find-package-with-library
          snow2-package-libraries
          gather-depends
          install
          uninstall
          client
          main-program)

  (import (scheme base) (scheme read) (scheme write)
          (scheme file) (scheme process-context))
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any fold)))
   (else (import (srfi 1))))
  (cond-expand
   (chibi (import (chibi filesystem)))
   (else))
  (import (snow snowlib)
          (snow srfi-13-strings)
          (seth srfi-69-hash-tables)
          (snow filesys) (snow binio) (snow genport) (snow zlib) (snow tar)
          (prefix (seth http) http-)
          (seth temporary-file)
          (seth string-read-write)
          (seth srfi-37-argument-processor))
  (begin

    (define-record-type <snow2-repository>
      (make-snow2-repository siblings packages local url)
      snow2-repository?
      (siblings snow2-repository-siblings set-snow2-repository-siblings!)
      (packages snow2-repository-packages set-snow2-repository-packages!)
      (local snow2-repository-local set-snow2-repository-local!)
      (url snow2-repository-url set-snow2-repository-url!))


    (define-record-type <snow2-sibling>
      (make-snow2-sibling name url trust)
      snow2-sibling?
      (name snow2-sibling-name set-snow2-sibling-name!)
      (url snow2-sibling-url set-snow2-sibling-url!)
      (trust snow2-sibling-trust set-snow2-sibling-trust!))


    (define-record-type <snow2-package>
      (make-snow2-package name url libraries repo)
      snow2-package?
      (name snow2-package-name set-snow2-package-name!)
      (url snow2-package-url set-snow2-package-url!)
      (libraries snow2-package-libraries set-snow2-package-libraries!)
      (repo snow2-package-repository set-snow2-package-repository!))


    (define-record-type <snow2-library>
      (make-snow2-library name path depends package)
      snow2-library?
      (name snow2-library-name set-snow2-library-name!)
      (path snow2-library-path set-snow2-library-path!)
      (depends snow2-library-depends set-snow2-library-depends!)
      (package snow2-library-package set-snow2-library-package!))

    (define (get-tag child)
      ;; extract the tag from an element that is assumed to be shaped like:
      ;; '(tag ...)
      (cond ((not (list? child))
             (error "not a list: ~A" child))
            ((null? child)
             (error "list is empty."))
            (else
             (car child))))


    (define (get-children-by-type obj child-type)
      ;; return any child sexps that is a list starting with child-type
      (filter (lambda (child)
                (eq? (get-tag child) child-type))
              (cdr obj)))


    (define (get-child-by-type obj child-type default)
      ;; find a non-optional child with the given tag.  the tag
      ;; is expected to be unique among the children.
      (let ((childs (get-children-by-type obj child-type)))
        (cond ((null? childs)
               (if default
                   default
                   (error "~A has no ~A\n" (get-tag obj) child-type)))
              ((> (length childs) 1)
               (error "~A has more than one ~A\n." obj child-type))
              (else
               (car childs)))))


    (define (get-string-by-type obj child-type default)
      ;; return the string from a child with the form
      ;; '(child-type "...")
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (string? result))
                        (error
                         "value of ~A in ~A isn't a string\n"
                         child-type (get-tag obj)))
                       (else
                        result)))))))


    (define (get-number-by-type obj child-type default)
      ;; return the number from a child with the form
      ;; '(child-type 1.0)
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (number? result))
                        (error
                         "value of ~A in ~A isn't a number\n"
                         child-type (get-tag obj)))
                       (else
                        result)))))))



    (define (get-list-by-type obj child-type default)
      ;; return the list from a child with the form
      ;; '(child-type (x y z))
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (list? result))
                        (error
                         "value of ~A in ~A isn't a list: ~A\n"
                         child-type (get-tag obj) result))
                       (else
                        result)))))))


    (define (get-args-by-type obj child-type default)
      ;; return the list '(x y z) from a child with the form
      ;; '(child-type x y z)
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              (else
               (cdr child)))))


    (define (depend-from-sexp depend-sexp)
      ;; depend-sexp will be a library name, like (snow snowlib)
      depend-sexp)


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
        (make-snow2-sibling name url trust)))


    (define (library-from-sexp library-sexp)
      ;; convert an s-exp into a library record
      (let ((name (get-list-by-type library-sexp 'name #f))
            (path (get-string-by-type library-sexp 'path #f))
            (depends-sexps (get-args-by-type library-sexp 'depends '())))
        (cond ((not name) #f)
              ((not path) #f)
              (else
               (make-snow2-library
                name path (map depend-from-sexp depends-sexps) #f)))))


    (define (package-from-sexp package-sexp)
      ;; convert a s-exp into a package record
      (let ((url (get-string-by-type package-sexp 'url #f))
            (name (get-list-by-type package-sexp 'name '()))
            (library-sexps (get-children-by-type package-sexp 'library)))
        (cond ((not url) #f)
              ((not name) #f)
              (else
               (let* ((libraries (map library-from-sexp library-sexps))
                      (package (make-snow2-package name url libraries #f)))
                 ;; backlink to packages
                 (for-each
                  (lambda (library)
                    (set-snow2-library-package! library package))
                  libraries)
                 package)))))


    (define (repository-from-sexp repository-sexp)
      ;; convert an s-exp into a repository record
      (cond ((not (list? repository-sexp))
             (error "repository definition isn't a list."))
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
                    (repo (make-snow2-repository siblings packages #f #f)))
               ;; backlink package to repository
               (for-each
                (lambda (package)
                  (set-snow2-package-repository! package repo))
                packages)
               repo))))


    (define (read-repository in-port)
      ;; read an s-exp from in-port and convert it to a repository record.
      (let* ((repository-sexp (read in-port)))
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
      (fold
       (lambda (package lst)
         (append (snow2-package-libraries package) lst))
       '()
       packages))


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


    (define (find-package-with-library repositories library-name)
      ;; find the last package that contains a library with the given name
      (let r-loop ((repositories repositories)
                   (candidate-packages '()))
        (cond
         ((null? repositories)
          (cond ((null? candidate-packages)
                 (error "couldn't find library" library-name))
                ;; XXX rather than just taking the last one,
                ;; select one based on version requirements, etc
                (else (car candidate-packages))))
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


    (define (find-packages-with-libraries repositories library-names)
      (let ((package-url-ht (make-hash-table)))
        (for-each
         (lambda (library-name)
           (let ((package
                  (find-package-with-library repositories library-name)))
             (cond ((not package)
                    (error "didn't find a package with library: ~S\n"
                           library-name))
                   (else
                    (hash-table-set!
                     package-url-ht (snow2-package-url package) package)))))
         library-names)
        (hash-table-values package-url-ht)))


    (define (library-from-name repositories library-name)
      (let* ((package (find-package-with-library repositories library-name)))
        (cond ((not package)
               (error
                "can't find package that contains ~S\n" library-name)
               #f)
              (else
               (let loop ((libraries (snow2-package-libraries package)))
                 (cond ((null? libraries) #f)
                       ((equal? library-name
                                (snow2-library-name (car libraries)))
                        (car libraries))
                       (else (loop (cdr libraries)))))))))


    (define (gather-depends repositories libraries)
      ;;
      ;; returns a list of snow2-packages
      ;;
      (let ((lib-name-ht (make-hash-table))
            (package-url-ht (make-hash-table)))
        (for-each
         (lambda (library)

           (let ((lib-name (snow2-library-name library)))
             (let ((package (find-package-with-library repositories lib-name)))
               (hash-table-set! lib-name-ht lib-name library)
               (hash-table-set! package-url-ht (snow2-package-url package)
                                package))

             (for-each
              (lambda (depend)
                (let* ((package (find-package-with-library repositories depend))
                       (libs (snow2-package-libraries package)))
                  (hash-table-set! package-url-ht
                                   (snow2-package-url package)
                                   package)
                  ;; XXX if the same lib is in more than one
                  ;; package there should be some reason to pick one
                  ;; over the other?
                  (for-each
                   (lambda (lib)
                     (hash-table-set! lib-name-ht
                                      (snow2-library-name lib) lib))
                   libs)))
              (snow2-library-depends library))))
         libraries)

        (if (= (length (hash-table-keys lib-name-ht)) (length libraries))
            ;; nothing new added this pass, so we've finished.
            (hash-table-values package-url-ht)
            ;; we found more, go around again.
            (gather-depends repositories (hash-table-values lib-name-ht)))))


    (define (get-repository repository-url)
      (cond ((and (> (string-length repository-url) 8)
                  (or (string-prefix? "http://" repository-url)
                      (string-prefix? "https://" repository-url)))
             ;; get repository over http
             (snow-with-exception-catcher
              (lambda (exn)
                (display "unable to fetch repository index: "
                         (current-error-port))
                (display repository-url (current-error-port))
                (newline (current-error-port))
                (display exn (current-error-port))
                (newline (current-error-port))
                #f)
              (lambda ()
                (let ((repository
                       (http-call-with-request-body
                        repository-url read-repository)))
                  (set-snow2-repository-local! repository #f)
                  (set-snow2-repository-url! repository repository-url)
                  repository))))
            (else
             ;; read from local filesystem
             (let* ((index-path (snow-make-filename repository-url "index.scm"))
                    (in-port (open-input-file index-path))
                    (repository (read-repository in-port)))
               (set-snow2-repository-local! repository #t)
               (set-snow2-repository-url! repository repository-url)
               (close-input-port in-port)
               repository))))


    (define (get-repositories-and-siblings repositories repository-urls)
      (define (make-repo-has-url? url)
        (lambda (repository)
          (equal? (snow2-repository-url repository) url)))
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
                            (get-repositories-and-siblings
                             repositories (cdr repository-urls))))))))))


    (define (write-tar-recs-to-disk tar-recs)
      (let loop ((tar-recs tar-recs))
        (cond ((null? tar-recs) #t)
              (else
               (let ((t (car tar-recs)))
                 (cond
                  ((eq? (tar-rec-type t) 'directory)
                   (snow-create-directory-recursive
                    (tar-rec-name t)))
                  ((eq? (tar-rec-type t) 'regular)
                   (cond ((or (snow-file-symbolic-link? (tar-rec-name t))
                              (snow-file-directory? (tar-rec-name t)))
                          (display "not overwriting " (current-error-port))
                          (display (tar-rec-name t) (current-error-port))
                          (newline (current-error-port)))
                         (else
                          (let ((hndl (binio-open-output-file
                                       (tar-rec-name t))))
                            (binio-write-subu8vector
                             (tar-rec-content t) 0
                             (bytevector-length (tar-rec-content t)) hndl)))))
                  (else
                   (error "unexpected file type in tar file")))
                 (loop (cdr tar-recs)))))))


    (define (install repositories library-names use-symlinks verbose)

      (define (install-from-tgz repo local-package-filename)
        (snow-with-exception-catcher
         (lambda (err)
           (display "Error -- " (current-error-port))
           (display local-package-filename (current-error-port))
           (display " " (current-error-port))
           (cond
            ((snow-error-condition? err)
             (display (snow-error-condition-msg err) (current-error-port)))
            (else
             (display err)))
           (newline (current-error-port))
           #f)
         (lambda ()
           (let* ((bin-port (binio-open-input-file
                             local-package-filename))
                  (zipped-p (genport-native-input-port->genport bin-port))
                  (unzipped-p (gunzip-genport zipped-p))
                  (tar-recs (tar-unpack-genport unzipped-p)))
             (genport-close-input-port unzipped-p)
             (write-tar-recs-to-disk tar-recs)))))

      (define (install-from-http repo package url)
        (let-values (((write-port local-package-filename)
                      (temporary-file)))
          (display "downloading ")
          (display (snow-filename-strip-directory url))
          (display " from ")
          (display (snow2-repository-url repo))
          (newline)

          (let ((download-success
                 (snow-with-exception-catcher
                  (lambda (exn)
                    (display "unable to install package: "
                             (current-error-port))
                    (display url (current-error-port))
                    (newline (current-error-port))
                    (display exn (current-error-port))
                    (newline (current-error-port))
                    #f)
                  (lambda ()
                    (http-download-file url write-port)))))

            (cond (download-success
                   (let ((success
                          (install-from-tgz repo local-package-filename)))
                     (delete-file local-package-filename)
                     success))))))


      (define (install-symlinks repo package package-local-directory)
        (let ((repo-local-name (get-snow2-repo-name package))
              (package-local-name
               (snow-filename-strip-directory package-local-directory)))

          (define (local-repo-sld-files repo)
            ;; drop repo path from the start of each filename, skip test
            ;; directory.
            (filter
             (lambda (filename)
               (let ((parts (snow-unmake-filename filename)))
                 (or (< (length parts) 2)
                     (not (equal? (car parts) package-local-name))
                     (not (equal? (cadr parts) "test")))))
             (map
              (lambda (filename)
                (substring filename
                           (+ 1 (string-length ;; + 1 eats extra /
                                 (snow-filename-strip-extension
                                  (snow2-repository-url repo))))
                           (string-length filename)))
              (snow-directory-subfiles package-local-directory '(regular)))))


          (for-each
           (lambda (filename)
             (cond ((string-suffix? ".sld" filename)
                    (let* ((link-name
                            (snow-make-filename
                             repo-local-name
                             (snow-filename-strip-trailing-directory-separator
                              (snow-filename-directory
                               (snow-filename-strip-trailing-directory-separator
                                (snow-filename-directory filename))))
                             (snow-filename-strip-directory filename)))
                           (link-dir (snow-filename-directory link-name))
                           (libfile-name
                            (snow-make-filename
                             (snow2-repository-url repo) filename)))
                      (display "linking ")
                      (display libfile-name)
                      (display " to ")
                      (display link-name)
                      (newline)

                      (snow-create-directory-recursive link-dir)
                      (cond ((or (snow-file-exists? link-name)
                                 (snow-file-symbolic-link? link-name))
                             (snow-delete-file link-name)))

                      (snow-create-symbolic-link
                       (cond ((snow-filename-relative? libfile-name)
                              ;; we are making a link in a subdirectory,
                              ;; so prepend the required number of ../
                              (let* ((link-parts
                                      (snow-unmake-filename link-name))
                                     (depth (length link-parts))
                                     (dots (make-list (- depth 1) "..")))
                                (apply snow-make-filename
                                       (reverse (cons libfile-name dots)))))
                             (else libfile-name))

                       link-name)

                      ))))
           (local-repo-sld-files repo))))


      (define (install-from-directory repo package url)
        (let* ((package-file (snow-filename-strip-directory url))
               (package-name (snow-filename-strip-extension package-file))
               (package-local-directory (snow-make-filename
                                         (snow2-repository-url repo)
                                         package-name))
               )
          (cond ((and use-symlinks
                      (snow-file-directory? package-local-directory))
                 (install-symlinks repo package package-local-directory))
                (else
                 (let ((local-package-filename (snow-make-filename
                                                (snow2-repository-url repo)
                                                package-file)))
                   (display "extracting ")
                   (display package-file)
                   (display " from ")
                   (display (snow2-repository-url repo))
                   (newline)
                   (install-from-tgz repo local-package-filename))))))

      (let* ((pkgs (find-packages-with-libraries repositories library-names))
             (libraries (snow2-packages-libraries pkgs))
             (packages (gather-depends repositories libraries)))
        (for-each
         (lambda (package)
           (let* ((package-repo (snow2-package-repository package))
                  (url (snow2-package-url package))
                  (success
                   (cond
                    ((snow2-repository-local package-repo)
                     (install-from-directory package-repo package url))
                    (else
                     (install-from-http package-repo package url)))))
             (cond
              ((not success)
               (display "Failed to install " (current-error-port))
               (display (snow2-package-name package)
                        (current-error-port))
               (display ", " (current-error-port))
               (display (snow2-package-url package)
                        (current-error-port))
               (newline (current-error-port))))))
         packages)))


    (define (uninstall repositories library-names)
      #f)


    (define (list-depends repositories library-names)
      ;; print out what library-name depends on
      (let* ((pkgs (find-packages-with-libraries repositories library-names))
             (libraries (snow2-packages-libraries pkgs))
             (packages (gather-depends repositories libraries)))
        (for-each
         (lambda (package)
           (for-each
            (lambda (library)
              (display (snow2-library-name library))
              (newline))
            (snow2-package-libraries package)))
         packages)))


    (define (filter-libraries libs search-term)
      (let loop ((libs libs)
                 (results '()))
        (cond ((null? libs) (reverse results))
              (else
               (let* ((lib (car libs))
                      (name-as-string
                       (write-to-string (snow2-library-name lib))))
                 (loop (cdr libs)
                       (if (string-contains-ci name-as-string search-term)
                           (cons lib results)
                           results)))))))


    (define (search-for-libraries repositories search-terms)
      (for-each
       (lambda (result)
         (display (snow2-library-name result))
         (newline))
       (let loop ((search-terms search-terms)
                  (libs (all-libraries repositories)))
         (if (null? search-terms) libs
             (loop (cdr search-terms)
                   (filter-libraries libs (car search-terms)))))))


    (define (all-libraries repositories)
      ;; make a list of all libraries in all repositories
      (let repo-loop ((repositories repositories)
                      (results '()))
        (cond ((null? repositories) results)
              (else
               (let pkg-loop ((packages (snow2-repository-packages
                                         (car repositories)))
                              (results results))
                 (cond ((null? packages)
                        (repo-loop (cdr repositories)
                                   results))
                       (else
                        (pkg-loop
                         (cdr packages)
                         (append results
                                 (snow2-package-libraries
                                  (car packages)))))))))))



    (define (client repository-urls operation library-names
                    use-symlinks verbose)
      (let ((repositories (get-repositories-and-siblings '() repository-urls)))

        (cond (verbose
               (display "repositories:\n" (current-error-port))
               (for-each
                (lambda (repository)
                  (display "  " (current-error-port))
                  (display (snow2-repository-url repository)
                           (current-error-port))
                  (newline (current-error-port)))
                repositories)))

        (cond ((equal? operation "install")
               (install repositories library-names use-symlinks verbose))
              ((equal? operation "uninstall")
               (uninstall repositories library-names))
              ((equal? operation "list-depends")
               (list-depends repositories library-names))
              (else
               (error "unknown snow2 client operation" operation)))))


    (define options
      (list
       (option '(#\r "repo") #t #f
               (lambda (option name arg operation repos
                               use-symlinks libs verbose)
                 (values operation
                         (reverse (cons arg (reverse repos)))
                         use-symlinks libs verbose)))

       (option '(#\s "symlink") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs verbose)
                 (values operation repos #t libs verbose)))

       (option '(#\v "verbose") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs verbose)
                 (values operation repos use-symlinks libs #t)))

       (option '(#\h "help") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs verbose)
                 (usage "")))))


    (define (usage msg)
      (let ((pargs (command-line)))
        (display msg (current-error-port))
        (display (car pargs) (current-error-port))
        (display " " (current-error-port))
        (display "[arguments] <operation> '(library name)' ...\n"
                 (current-error-port))
        (display "  <operation> can be \"install\" or " (current-error-port))
        (display "\"uninstall\" or \"list-depends\" or \"search\"\n"
                 (current-error-port))
        (display "  -r --repo <url>      " (current-error-port))
        (display "Prepend to list of snow2 repositories.\n"
                 (current-error-port))
        (display "  -s --symlink         " (current-error-port))
        (display "Make symlinks to a repo's source files.\n")
        (display "  -v --verbose         " (current-error-port))
        (display "Print more.\n" (current-error-port))
        (display "  -h --help            " (current-error-port))
        (display "Print usage message.\n" (current-error-port))
        (display "\nExample: snow2 install '(snow srfi-13-strings)'\n")
        (exit 1)))


    (define (read-library-name library-name-argument)
      (snow-with-exception-catcher
       (lambda (exn)
         (usage
          (string-append
           "\nincorrectly formatted library-name argument: \""
           library-name-argument
           "\"\n\n")))
       (lambda ()
         (read-from-string library-name-argument))))


    (define (main-program)
      (let-values
          (((operation repository-urls use-symlinks libs-or-st verbose)
            (args-fold
             (cdr (command-line))
             options
             ;; unrecognized
             (lambda (option name arg . seeds)
               ;; (error "Unrecognized option:" name)
               (usage (string-append "Unrecognized option:"
                                     (if (string? name) name (string name))
                                     "\n\n")))
             ;; operand (arguments that don't start with a hyphen)
             (lambda (operand operation repos use-symlinks libs verbose)
               (if operation
                   (values operation repos use-symlinks
                           (cons operand libs) verbose)
                   (values operand repos use-symlinks libs verbose)))
             #f ;; initial value of operation
             '() ;; initial value of repos
             #f ;; initial value of use-symlinks
             '() ;; initial value of libs
             #f ;; initial value of verbose
             )))
        (let ((repository-urls
               (if (null? repository-urls)
                   '("http://snow2.s3-website-us-east-1.amazonaws.com/")
                   repository-urls)))
          (cond ((not operation) (usage ""))
                ;; search operation
                ((member operation '("search"))
                 (let ((repositories (get-repositories-and-siblings
                                      '() repository-urls)))
                   (search-for-libraries repositories libs-or-st)))
                ;; other operations
                ((not (member operation '("link-install"
                                          "install"
                                          "uninstall"
                                          "list-depends"
                                          )))
                 (usage (string-append "Unknown operation: "
                                       operation "\n\n")))
                (else
                 (let ((library-names (map read-library-name libs-or-st)))
                   (cond (verbose
                          (display "libraries to install:\n"
                                   (current-error-port))
                          (write library-names)
                          (newline)))
                   (client repository-urls operation
                           library-names use-symlinks verbose))
                 )))))))

;; "http://snow2.s3-website-us-east-1.amazonaws.com/"
;; "http://snow-repository.s3-website-us-east-1.amazonaws.com/"
