(define-library (seth snow2 client)
  (export install
          uninstall
          main-program)

  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context))
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any fold)))
   (else (import (srfi 1))))
  (cond-expand
   (chibi (import (chibi filesystem)))
   (else))
  (import (srfi 13)
          (snow filesys) (snow binio) (snow genport) (snow zlib) (snow tar)
          (srfi 27)
          (srfi 29)
          (prefix (seth http) http-)
          (seth temporary-file)
          (seth string-read-write)
          (srfi 37)
          (seth uri)
          (seth crypt md5)
          (seth snow2 types)
          (seth snow2 utils)
          (seth snow2 r7rs-library)
          (seth snow2 manage)
          )
  (begin


    (define (display-error msg err . maybe-depth)
      (let* ((depth (if (pair? maybe-depth) (car maybe-depth) 0))
             (depth-s (make-string (* depth 2) #\space)))
        (display  depth-s)
        (display "Error -- ")
        (display msg)
        (display " ")
        (write (error-object-message err))
        (newline)
        (write (error-object-irritants err))
        (for-each (lambda (irr)
                    (cond ((error-object? irr)
                           (display-error "" irr (+ depth 1)))
                          (else
                           (display depth-s)
                           (write irr (current-error-port))
                           (newline (current-error-port)))))
                  (error-object-irritants err))))


    (define (write-tar-recs-to-disk tar-recs paths-to-extract)

      (let loop ((tar-recs tar-recs))
        (cond ((null? tar-recs) #t)
              (else
               (let ((t (car tar-recs)))

                 ;; the package tgz files are all contained within a single
                 ;; toplevel directory.  when we install the package for use,
                 ;; we pretend they aren't.
                 ;; XXX this is a hack, do something better here.
                 (let* ((path (snow-split-filename (tar-rec-name t)))
                        (path-sans-container (cdr path))
                        (name-san-container
                         (snow-combine-filename-parts path-sans-container)))
                   (cond
                    ;; the tar file will contain files we don't want
                    ;; to install.  skip over anything that doesn't
                    ;; match something in paths-to-extract
                    ((not (member path-sans-container paths-to-extract)) #t)

                    ((eq? (tar-rec-type t) 'regular)
                     (tar-rec-name-set! t name-san-container)
                     ;; create the directory that contains this file
                     (let* ((path (snow-split-filename (tar-rec-name t)))
                            (parent-path (reverse (cdr (reverse path)))))
                       (if (not (null? parent-path))
                           (snow-create-directory-recursive
                            (snow-combine-filename-parts parent-path))))

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
                               (bytevector-length (tar-rec-content t)) hndl)
                              (close-output-port hndl)))))

                    (else
                     (error "unexpected file type in tar file")))
                   (loop (cdr tar-recs))))))))

    (define (install repositories library-names use-symlinks steps verbose)
      ;; this is the main interface point for downloading/finding and
      ;; unpacking packages.  repositories is a list of repository records.
      ;; library-names is a list of library-name s-expressions.
      ;; use-symlinks being true will cause symlinks to source files rather
      ;; than copies (when possible).  verbose prints more.


      (define (install-from-tgz repo package local-package-tgz-file)

        (guard
         (err (#t
               (display-error local-package-tgz-file err)
               (raise err)))
         (let* ((pkg-tgz-size (snow2-package-size package))
                (checksum (snow2-package-size package))
                (pkg-md5-sum (cond ((and checksum
                                         (pair? checksum)
                                         (eq? (car checksum) 'md5))
                                    (cadr checksum))
                                   (else #f))))

           (let* ((zipped-p (genport-open-input-file local-package-tgz-file))
                  (unzipped-p (gunzip-genport zipped-p))
                  (tar-data (genport-read-u8vector unzipped-p)))

             (genport-close-input-port unzipped-p)

             ;; if the package metadata had (size ...) or (checksum ...)
             ;; make sure the provided values match those of what we've
             ;; un-gzipped.
             (cond ((and pkg-md5-sum (not (eq? pkg-md5-sum (md5 tar-data))))
                    (display "Error: checksum mismatch on ")
                    (display (uri->string (snow2-package-url package)))
                    (display " (")
                    (display local-package-tgz-file)
                    (display ") -- expected ")
                    (write pkg-md5-sum)
                    (display " and got ")
                    (write (md5 tar-data))
                    (newline)
                    (exit 1))

                   ((and (number? pkg-tgz-size)
                         (not (= pkg-tgz-size (bytevector-length tar-data))))
                    (display "Error: size mismatch on ")
                    (display (uri->string (snow2-package-url package)))
                    (display " (")
                    (display local-package-tgz-file)
                    (display ") -- expected ")
                    (write pkg-tgz-size)
                    (display " and got ")
                    (write (bytevector-length tar-data))
                    (newline)
                    (exit 1)))

             (let* ((tarred-p (genport-open-input-u8vector tar-data))
                    (tar-recs (tar-unpack-genport tarred-p))
                    ;; the package contains files that may not be needed
                    ;; for this "step".  The only two steps this code
                    ;; currently supports are "test" and "final".
                    ;; libs-for-step is the list of snow2-libraries from
                    ;; this package that are for use in the given steps.
                    (libs-for-steps (find-libraries-for-steps package steps)))
               (genport-close-input-port tarred-p)

               (for-each 
                (lambda (lib)
                  (let* ((file-for-step (snow2-library-path lib))
                         (path-for-step (snow-split-filename file-for-step)))
                    ;; extract the main .sld file for each library
                    (write-tar-recs-to-disk tar-recs (list path-for-step))

                    ;; examine each extracted .sld file and see if they
                    ;; include other files
                    (let* ((lib-filename
                            (snow-combine-filename-parts path-for-step))
                           ;; read the library s-exp back in
                           (lib-sexp (r7rs-library-file->sexp lib-filename))
                           ;; find included files
                           (included-files
                            (filter
                             (lambda (included-file)
                               (not (equal? included-file lib-filename)))
                             (r7rs-get-library-manifest lib lib-sexp)))
                           ;; convert to path-part lists
                           (included-paths
                            (map snow-split-filename included-files)))
                      ;; write out files included by this library
                      (write-tar-recs-to-disk tar-recs included-paths))))
                libs-for-steps))))))


      (define (install-from-http repo package url)
        (let-values (((write-port local-package-tgz-file)
                      (temporary-file #t)))
          (display "downloading ")
          (display (snow-filename-strip-directory (uri->string url)))
          (display " from ")
          (display (uri->string (snow2-repository-url repo)))
          (newline)

          (let ((download-success
                 (guard
                  (err (#t
                        (display-error
                         (string-append
                          "Unable to install package: "
                          (uri->string url))
                         err)
                        (raise err)))
                  (http-download-file (uri->string url) write-port))))

            (cond (download-success
                   (let ((success (install-from-tgz
                                   repo package local-package-tgz-file)))
                     (delete-file local-package-tgz-file)
                     success))
                  (else #f)))))


      (define (install-symlinks local-repository package)
        (let* ((libraries (snow2-package-libraries package))
               (lib-sexps (map (lambda (lib)
                                 (let* ((lib-filename
                                         (local-repository->in-fs-lib-filename
                                          local-repository lib)))
                                   (r7rs-library-file->sexp lib-filename)))
                               libraries))
               (manifest (fold append '()
                               (map r7rs-get-library-manifest
                                    libraries lib-sexps)))
               (repo-path (uri-path (snow2-repository-local local-repository))))
          (for-each
           (lambda (library-member-filename)
             (let* ((dst-path (snow-split-filename library-member-filename))
                    (dst-filename (snow-combine-filename-parts dst-path))
                    (dst-dir-path (reverse (cdr (reverse dst-path))))
                    (dst-dirname (snow-combine-filename-parts dst-dir-path))
                    (src-path (append repo-path dst-path))
                    (src-filename (snow-combine-filename-parts src-path)))

             ;; (display "src-path=") (write src-path) (newline)
             ;; (display "src-filename=") (write src-filename) (newline)
             ;; (display "dst-path=") (write dst-path) (newline)
             ;; (display "dst-filename=") (write dst-filename) (newline)
             ;; (display "dst-dir-path=") (write dst-dir-path) (newline)
             ;; (display "dst-dirname=") (write dst-dirname) (newline)

             (snow-create-directory-recursive dst-dirname)

             (cond ((or (file-exists? dst-filename)
                        (snow-file-symbolic-link? dst-filename))
                    (delete-file dst-filename)))

               (snow-create-symbolic-link
                (cond ((snow-filename-relative? src-filename)
                       ;; we are making a link in a subdirectory,
                       ;; so prepend the required number of ../
                       (let* ((link-parts (snow-split-filename dst-filename))
                              (depth (length link-parts))
                              (dots (make-list (- depth 1) "..")))
                         (apply snow-make-filename
                                (reverse (cons src-filename dots)))))
                      (else src-filename))
                dst-filename)))
           manifest)))


      (define (install-from-directory repo package url)
        (let ((local-package-tgz-file
               (snow-combine-filename-parts
                (local-repository->in-fs-tgz-path repo package))))
          (display "extracting ")
          (display (snow2-package-get-readable-name package))
          (display " from ")
          (display local-package-tgz-file)
          (newline)
          (install-from-tgz repo package local-package-tgz-file)))


      (let* ((pkgs (find-packages-with-libraries repositories library-names))
             (libraries (snow2-packages-libraries pkgs))
             (packages (gather-depends repositories libraries)))
        (for-each
         (lambda (package)
           (let* ((package-repo (snow2-package-repository package))
                  (success
                   (cond
                    ;; local repository, use symlinks
                    ((and (snow2-repository-local package-repo) use-symlinks)
                     (install-symlinks package-repo package))
                    ;; local repository, use tgz files
                    ((snow2-repository-local package-repo)
                     (install-from-directory
                      package-repo package
                      (snow2-repository-local package-repo)))
                    ;; remote repository
                    (else
                     (install-from-http
                      package-repo package
                      (snow2-package-absolute-url package))))))
             (cond
              ((not success)
               (display "Failed to install " (current-error-port))
               (display (snow2-package-name package)
                        (current-error-port))
               (display ", " (current-error-port))
               (display (uri->string (snow2-package-url package))
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


    (define options
      (list
       (option '(#\r "repo") #t #f
               (lambda (option name arg operation repos
                               use-symlinks libs test verbose)
                 (values operation
                         (reverse (cons (uri-reference arg) (reverse repos)))
                         use-symlinks libs test verbose)))

       (option '(#\s "symlink") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs test verbose)
                 (values operation repos #t libs test verbose)))

       (option '(#\t "test") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs test verbose)
                 (values operation repos use-symlinks libs #t verbose)))

       (option '(#\v "verbose") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs test verbose)
                 (values operation repos use-symlinks libs test #t)))

       (option '(#\h "help") #f #f
               (lambda (option name arg operation repos
                               use-symlinks libs test verbose)
                 (usage "")))))


    (define (usage msg)
      (let ((pargs (command-line)))
        (display msg (current-error-port))
        (display (car pargs) (current-error-port))
        (display " " (current-error-port))
        (display "[arguments] <operation> '(library name)' ...\n"
                 (current-error-port))
        (display "  <operation> can be one of: install " (current-error-port))
        (display "uninstall list-depends " (current-error-port))
        (display "search\n" (current-error-port))
        (display "  -r --repo <url>      " (current-error-port))
        (display "Add to list of snow2 repositories.\n"
                 (current-error-port))
        (display "  -s --symlink         " (current-error-port))
        (display "Make symlinks to a repo's source files.\n"
                 (current-error-port))
        (display "  -t --test            " (current-error-port))
        (display "Install code needed to run tests.\n"
                 (current-error-port))
        (display "  -v --verbose         " (current-error-port))
        (display "Print more.\n" (current-error-port))
        (display "  -h --help            " (current-error-port))
        (display "Print usage message.\n" (current-error-port))
        (display "\nExample: snow2 install '(snow hello)'\n"
                 (current-error-port))
        (display "\nsee "
                 (current-error-port))
        (display "https://github.com/sethalves/snow2-client#snow2-client\n"
                 (current-error-port))
        (exit 1)))


    (define (read-library-name library-name-argument)
      (guard
       (err (#t
             (usage
              (string-append
               "\nincorrectly formatted library-name argument: \""
               library-name-argument
               "\"\n\n"))))
       (read-from-string library-name-argument)))


    (define (main-program)
      (random-source-randomize! default-random-source)
      (let-values
          (((operation repository-urls use-symlinks args test verbose)
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
             (lambda (operand operation repos use-symlinks libs test verbose)
               (if operation
                   (values operation repos use-symlinks
                           (cons operand libs) test verbose)
                   (values operand repos use-symlinks libs test verbose)))
             #f ;; initial value of operation
             '() ;; initial value of repos
             #f ;; initial value of use-symlinks
             '() ;; initial value of args
             #f ;; initial value of test
             #f ;; initial value of verbose
             )))
        (let* ((default-repo-url
                 "http://snow2.s3-website-us-east-1.amazonaws.com/index.scm")
               (repository-urls
                (if (null? repository-urls)
                    (list (uri-reference default-repo-url))
                    repository-urls))
               (repositories
                (let ((cached-repositories #f))
                  (lambda ()
                    (cond (cached-repositories cached-repositories)
                          (else
                           (set! cached-repositories
                                 (get-repositories-and-siblings
                                  '() repository-urls))
                           cached-repositories)))))
               (steps (if test '(test final) '(final)))
               (credentials #f))

          (cond (verbose
                 (display "repositories:\n" (current-error-port))
                 (for-each
                  (lambda (repository)
                    (display "  " (current-error-port))
                    (display (uri->string (snow2-repository-url repository))
                             (current-error-port))
                    (newline (current-error-port)))
                  (repositories))))

          (cond
           ((not operation) (usage ""))

           ;; search operation
           ((member operation '("search"))
            (search-for-libraries (repositories) args))

           ;; tar up and gzip a package
           ((member operation '("package"))
            (make-package-archives (repositories) args verbose))

           ;; run tests in a source repository
           ((member operation '("run-source-tests"))
            (run-source-tests (repositories) args verbose))

           ;; upload a tgz package file
           ((member operation '("s3-upload" "upload-s3" "upload"))
            (upload-packages-to-s3 credentials (repositories)
                                   args verbose))

           ;; repository source sanity checker
           ((member operation '("check" "lint"))
            (for-each sanity-check-repository (repositories))
            (check-packages credentials (repositories) args verbose))

           ;; librarys operations
           (else
            (let ((library-names (map read-library-name args)))

              (cond (verbose
                     (display "libraries:\n" (current-error-port))
                     (write library-names)
                     (newline)))

              (cond
               ;; install libraries and dependencies
               ((equal? operation "install")
                (install (repositories) library-names
                         use-symlinks steps verbose))

               ;; uninstall libraries
               ((equal? operation "uninstall")
                (uninstall (repositories) library-names))

               ;; list what a library depends on
               ((member operation '("list-dep" "list-depends"))
                (list-depends (repositories) library-names))

               ;; unknown operation
               (else
                (usage (string-append "Unknown operation: "
                                      operation "\n\n"))))))))))))
