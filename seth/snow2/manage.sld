(define-library (seth snow2 manage)
  (export make-package-archives
          run-source-tests
          upload-packages-to-s3
          check-packages)

  (import (scheme base)
          (scheme eval)
          (scheme read)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme time)
          (scheme process-context)
          )
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any
                        fold last lset-difference delete-duplicates
                        drop-right find)))
   (else (import (srfi 1))))
  (import (snow bytevector)
          (snow tar)
          (snow zlib)
          (snow filesys)
          (srfi 13)
          (snow extio)
          (srfi 29)
          (seth uri)
          (seth crypt md5)
          (seth aws common)
          (seth aws s3)
          (seth snow2 types)
          (seth snow2 utils)
          (seth snow2 r7rs-library)
          (srfi 95)
          (seth string-read-write)
          )
  (cond-expand
   (gauche (import (srfi gauche-95)))
   (else))

  (begin

    (define (make-package-archive
             repositories local-repository package-metafile package verbose)
      ;; create the .tgz file that gets uploaded to a repository.
      ;; update the size and md5 sum and depends in the package meta-data
      ;; in index.scm.

      (cond
       (verbose
        (display "-- packaging ")
        (write (uri->string (snow2-package-url package)))
        (newline)))

      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             ;; put everything inside a toplevel directory to avoid
             ;; the resulting tgz being a tar bomb.
             (container-dirname
              (string-append (snow2-package-get-readable-name package)
                             "-" (snow2-package-version package))))

        (define (lib-file->tar-recs lib-filename)
          ;; create a tar-rec for a file
          (let* ((lib-rel-path (snow-split-filename lib-filename))
                 (lib-full-path (repo-path->file-path repo-path lib-rel-path))
                 (lib-full-filename
                  (snow-combine-filename-parts lib-full-path)))
            (cond ((not (file-exists? lib-full-filename))
                   (display "package source file missing: ")
                   (display lib-full-filename)
                   (newline)
                   (exit 1))
                  (else
                   (let ((tar-recs (tar-read-file lib-full-filename)))
                     (cond ((not (= (length tar-recs) 1))
                            (error "unexpected tar-rec count"
                                   lib-filename tar-recs)))
                     (tar-rec-name-set!
                      (car tar-recs)
                      (snow-combine-filename-parts
                       (cons container-dirname lib-rel-path)))
                     (car tar-recs))))))

        (define (lib-dir->tar-recs lib-dirname)
          ;; create a tar-rec for a directory
          (let* ((lib-rel-path (snow-split-filename lib-dirname))
                 (tar-rec
                  (make-tar-rec
                   (snow-combine-filename-parts
                    (append (list container-dirname) lib-rel-path (list "")))
                   493 ;; mode
                   0 ;; uid
                   0 ;; gid
                   (snow-file-mtime
                    (snow-combine-filename-parts
                     (repo-path->file-path repo-path lib-rel-path)))
                   'directory
                   "" ;; linkname
                   "root" ;; uname
                   "root" ;; gname
                   0 ;; devmajor
                   0 ;; devminor
                   #f ;; atime
                   #f ;; ctime
                   (make-bytevector 0))))
            (list tar-rec)))

        (define (file->directories filename)
          ;; given a relative filename, return a list of directory
          ;; paths.  if filename is "a/b/c", return '("a/" "a/b/")
          (let loop ((parts (drop-right (snow-split-filename filename) 1))
                     (result '()))
            (cond ((null? parts)
                   (map snow-combine-filename-parts result))
                  (else
                   (loop (drop-right parts 1)
                         (cons parts result))))))

        (define (manifest->directories manifest)
          ;; product a list of directories needed in order to hold
          ;; all the files listed in manifest.
          (delete-duplicates
           (fold append '() (map file->directories manifest))
           equal?))

        (define (make-package-meta-tar-rec)
          ;; make a tar-rec for the metafile for the package.  set
          ;; the name so it ends up in the same place as in chibi's
          ;; packages: container-dirname/package.scm
          (let ((rel-meta-name (snow-combine-filename-parts
                                (list container-dirname "package.scm")))
                (tar-recs (tar-read-file package-metafile)))
            (cond ((not (= (length tar-recs) 1))
                   (error "unexpected tar-rec count"
                          rel-meta-name tar-recs)))
            (tar-rec-name-set! (car tar-recs) rel-meta-name)
            (car tar-recs)))


        (let* ((libraries (snow2-package-libraries package))
               ;; read in the files indicated by the (path ...) clauses
               ;; in the library definitions.
               (lib-sexps (map (lambda (lib)
                                 (let* ((lib-filename
                                         (local-repository->in-fs-lib-filename
                                          local-repository lib)))
                                   (r7rs-library-file->sexp lib-filename)))
                               libraries))
               ;; get a list of filenames for all libraries in the package
               (manifest (fold append '()
                               (map r7rs-get-library-manifest
                                    libraries lib-sexps)))
               ;; get a list of directories needed to hold the library files
               (dirs (manifest->directories manifest))
               ;; make tar records for the directories
               (dir-tar-recs (fold append '() (map lib-dir->tar-recs dirs)))
               ;; make tar records for the files
               (file-tar-recs (map lib-file->tar-recs manifest))
               ;; throw in a package.scm file
               (package-meta-tar-rec (make-package-meta-tar-rec))
               ;; combine them
               (all-tar-recs (append dir-tar-recs
                                     (list package-meta-tar-rec)
                                     file-tar-recs))
               ;; figure out the name of the tgz file within the local repo
               (package-url (snow2-package-absolute-path package))
               (package-filename
                (if (and package-url (pair? (uri-path package-url)))
                    (last (uri-path package-url))
                    "unknown.tgz"))
               (local-package-path
                (repo-path->file-path repo-path (list package-filename)))
               (local-package-filename
                (snow-combine-filename-parts local-package-path)))

          ;; get a list of libraries this package depends on and
          ;; set library names.
          (for-each
           (lambda (lib lib-sexp)
             ;; if the depends have changes from what's in index.scm
             ;; mark the package dirty
             (let* ((all-deps (r7rs-get-imported-library-names
                               lib-sexp verbose))
                    ;; remove dependencies that we can't find
                    ;; packages for.
                    (deps
                     (filter
                      (lambda (dep-lib-name)
                        (find-package-with-library
                         repositories dep-lib-name))
                      all-deps)))

               (define (lib-name<? a b)
                 (string-ci<? (write-to-string a)
                              (write-to-string b)))

               (cond
                ((not (equal? (sort (snow2-library-depends lib) lib-name<?)
                              (sort deps lib-name<?)))
                 (set-snow2-library-depends! lib deps)
                 (set-snow2-package-dirty! package #t)
                 (set-snow2-repository-dirty! local-repository #t)

                 (cond (verbose
                        (display "  setting depends to ")
                        (write deps)
                        (newline))))))

             (cond ((or (not (snow2-library-name lib))
                        (null? (snow2-library-name lib)))
                    (cond (verbose
                           (display "  setting library name to ")
                           (write (lib-sexp->name lib-sexp))
                           (newline)))
                    (set-snow2-package-dirty! package #t)
                    (set-snow2-repository-dirty! local-repository #t)
                    (set-snow2-library-name!
                     lib (lib-sexp->name lib-sexp)))
                   (else
                    (cond (verbose
                           (display "  keeping library name of ")
                           (write (snow2-library-name lib))
                           (newline))))))
           libraries lib-sexps)

          ;; find the most recent file mtime and set the mtimes of
          ;; all the directories to this.  this keeps the md5 of
          ;; the .tgz file from changing as often.
          (let ((newest-mtime (apply max (map tar-rec-mtime file-tar-recs))))
            (for-each
             (lambda (dir-tar-rec)
               (tar-rec-mtime-set! dir-tar-rec newest-mtime))
             dir-tar-recs))

          (display "writing ")
          (display local-package-path)
          (newline)

          ;; if the .tgz file already exists, delete it
          (cond ((or (file-exists? local-package-filename)
                     (snow-file-symbolic-link? local-package-filename))
                 (delete-file local-package-filename)))

          ;; save out the new .tgz file
          (let* ((tar-data (tar-pack-u8vector all-tar-recs))
                 (tgz-data (gzip-u8vector tar-data))
                 (out-p (open-binary-output-file local-package-filename)))

            (write-bytevector tgz-data out-p)
            (close-output-port out-p)

            (let ((local-package-md5 (bytes->hex-string (md5 tar-data)))
                  (local-package-size (bytevector-length tar-data)))

              (display "  size=")
              (write local-package-size)
              (display " md5=")
              (write local-package-md5)
              (newline)

              (cond ((or (not (number? (snow2-package-size package)))
                         (not (= (snow2-package-size package)
                                 local-package-size)))
                     (set-snow2-package-size! package local-package-size)
                     (set-snow2-package-dirty! package #t)
                     (set-snow2-repository-dirty! local-repository #t)
                     (display "setting package dirty due to size\n")))
              (cond ((not (equal? (snow2-package-checksum package)
                                  `(md5 ,local-package-md5)))
                     (set-snow2-package-checksum!
                      package `(md5 ,local-package-md5))
                     (set-snow2-package-dirty! package #t)
                     (set-snow2-repository-dirty! local-repository #t)
                     (display "setting package dirty due to md5\n"))))))))


    (define (conditional-put-object! credentials bucket s3-path local-filename)
      ;; ask s3 for the md5-sum of the file we're about to upload.  if
      ;; they don't match, upload the new file.
      (let ((local-md5 (filename->md5 local-filename))
            (local-p (open-binary-input-file local-filename))
            (md5-on-s3 (get-object-md5 credentials bucket s3-path)))

        ;; (display "local-md5=") (write local-md5) (newline)
        ;; (display "md5-on-s3=") (write md5-on-s3) (newline)

        (cond ((equal? md5-on-s3 local-md5)
               (display "[")
               (write local-filename)
               (display " unchanged]\n"))
              (else
               (display "[")
               (write local-filename)
               (display " --> s3:")
               (display bucket)
               (display s3-path)
               (display "]\n")
               (guard
                (err (#t
                      (display "error uploading ")
                      (write local-filename)
                      (display " to s3.\n")
                      (write (error-object-message err))
                      (newline)
                      (write (error-object-irritants err))
                      (newline)
                      (raise err)))
                (put-object! credentials bucket s3-path local-p
                             #f ;; (snow-file-size local-filename)
                             (if (string-suffix? ".html" s3-path)
                                 "text/html"
                                 "application/octet-stream")
                             'public-read))
               (close-input-port local-p)))))


    (define (upload-package-to-s3 credentials local-repository package)
      (let ((url (snow2-package-absolute-url package)))
        (cond (url
               (let* ((bucket (uri->bucket url))
                      (s3-path (uri->path-string url)))
                 (cond (bucket
                        (conditional-put-object!
                         (if credentials credentials
                             (get-credentials-for-s3-bucket bucket))
                         bucket s3-path
                         (local-repository->in-fs-tgz-filename
                          local-repository package)))
                       (else
                        (error "unable to determine s3 bucket from url"
                               (uri->string (snow2-package-url package)))))))
              (else
               (error "can't determine packages absolute url."
                      (uri->string (snow2-repository-url local-repository))
                      (uri->string (snow2-package-url package)))))))


    (define (find-implied-local-repository)
      ;; figure out which repository (if any) the user's CWD was within when
      ;; this program was run.
      (or
       (get-repository
        (uri-reference (snow-combine-filename-parts '("."))) #f)
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".."))) #f)
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".."))) #f)
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".." ".."))) #f)
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".." ".." ".."))) #f)
       ))


    (define (all-package-metafiles local-repository)
      ;; return a list of package-metafile filenames for the given
      ;; local repository
      (let* ((repo-path (uri-path (snow2-repository-local local-repository)))
             (repo-dirname (snow-combine-filename-parts repo-path))
             (packages-dirname (snow-make-filename repo-dirname "packages")))
        (map (lambda (package-filename)
               (snow-make-filename packages-dirname package-filename))
             (filter
              (lambda (filename)
                (not (string-suffix? "~" filename)))
              (snow-directory-files packages-dirname)))))


    (define (resolve-package-metafile local-repository package-metafile)
      ;; package-file may be a path (absolute or relative) to
      ;; a repo/packages/NAME.package file, or it may be
      ;; just NAME.  either way, return the former.
      (cond ((string-suffix? ".package" package-metafile) package-metafile)
            (else
             (let* ((repo-uri (snow2-repository-local local-repository))
                    (repo-path (uri-path repo-uri)))
               (snow-combine-filename-parts
                (repo-path->file-path
                 repo-path
                 (list "packages" (string-append
                                   package-metafile
                                   ".package"))))))))



    (define (resolve-package-metafiles local-repository package-metafiles)
      ;; call resolve-package-metafile for each of package-metafiles
      (map (lambda (package-file)
             (resolve-package-metafile local-repository package-file))
           package-metafiles))


    (define (local-repository-operation repositories op verbose)
      ;; decide which local repository is intended.
      ;; call (op local-repository)
      (let ((repositories (filter snow2-repository-local repositories)))
        (let ((repository
               (cond ((> (length repositories) 1)
                      (let ((repo (last repositories)))
                        (display "multiple local repositories given, using ")
                        (write (uri->string (snow2-repository-url repo)))
                        (newline)
                        repo))
                     ((= (length repositories) 1)
                      (car repositories))
                     (else (find-implied-local-repository)))))
          (cond (repository
                 (set-snow2-repository-dirty! repository #t)
                 (sanity-check-repository repository)
                 (let ((result (op repository)))
                   (sanity-check-repository repository)
                   (cond ((snow2-repository-dirty repository)

                          ;; update index.scm if the repository is "dirty"
                          (let ((index-scm-filename
                                 (local-repository->in-fs-index-filename
                                  repository)))
                            (if (file-exists? index-scm-filename)
                                (delete-file index-scm-filename))
                            (let ((p (open-output-file index-scm-filename)))
                              (cond (verbose
                                     (display "rewriting ")
                                     (write index-scm-filename)
                                     (newline)))
                              (snow-pretty-print (repository->sexp repository) p)
                              (close-output-port p)))

                          ;; update index.html if the repository is "dirty"
                          (let* ((index-html-filename
                                  (local-repository->in-fs-html-filename
                                   repository)))
                            (if (file-exists? index-html-filename)
                                (delete-file index-html-filename))
                            (let ((p (open-output-file index-html-filename)))
                              (cond (verbose
                                     (display "rewriting ")
                                     (write index-html-filename)
                                     (newline)))
                              (write-string (repository->html repository) p)
                              (close-output-port p)))

                          ;; we've rewritten the repo files, no longer dirty.
                          (set-snow2-repository-dirty! repository #f)))
                   result))
                (else
                 (error
                  "Unable to determine which repository to operate on."))))))


    (define (set-local-lib-name local-repository lib)
      (let* ((lib-filename
              (local-repository->in-fs-lib-filename local-repository lib))
             (lib-sexp (r7rs-library-file->sexp lib-filename))
             (lib-name (lib-sexp->name lib-sexp)))
        (cond ((not (equal? lib-name (snow2-library-name lib)))
               (set-snow2-library-name! lib (lib-sexp->name lib-sexp))
               #t)
              (else #f))))


    (define (local-packages-operation repositories package-metafiles op verbose)
      ;; decide which local repository is intended.
      ;; call (op local-repo package) for each package-file.
      ;; return a list of results.
      (local-repository-operation
       repositories
       (lambda (local-repository)
         (let* ((package-metafiles
                 (cond ((pair? package-metafiles) package-metafiles)
                       (else
                        (all-package-metafiles local-repository))))
                (package-metafiles
                 (resolve-package-metafiles local-repository package-metafiles))
                (result
                 (map
                  (lambda (package-metafile)
                    (let* ((package
                            (refresh-package-from-filename
                             local-repository package-metafile verbose)))

                      ;; XXX check return value here, set dirty?
                      (map
                       (lambda (lib) (set-local-lib-name local-repository lib))
                       (snow2-package-libraries package))

                      (op local-repository package-metafile package)))
                  package-metafiles)))
           result))
       verbose))


    (define (make-package-archives repositories package-metafiles verbose)
      ;; call make-package-archive for each of packages-files
      (local-packages-operation
       repositories package-metafiles
       (lambda (local-repository package-metafile package)
         (refresh-package-from-filename
          local-repository package-metafile verbose)
         (make-package-archive
          (cons local-repository repositories)
          local-repository package-metafile package verbose))
       verbose))


    (define (run-source-tests repositories package-metafiles verbose)
      ;; find the names of any libraries in the package that are
      ;; part of the 'test step.  set up an environment which
      ;; imports the test libraries and eval '(run-tests)
      (define (build-test-environment test-lib-names)
        (guard
         (err (#t
               (cond (verbose
                      (write (error-object-message err))
                      (newline)
                      (write (error-object-irritants err))
                      (newline)
                      (raise err))
                     (else #f))))
         (begin
           (cond (verbose
                  (newline)
                  (display "test-lib-names: ")
                  (write test-lib-names)
                  (newline)))
           (let ((result #f))
             (with-output-to-string
               (lambda ()
                 (set! result (apply environment test-lib-names))))
             result))))


      (define (get-test-lib-names test-libs)
        ;; (cond (verbose
        ;;        (newline)
        ;;        (for-each
        ;;         (lambda (test-lib)
        ;;           (write (snow2-library-path test-lib))
        ;;           (display " --> ")
        ;;           (write (snow2-library-name test-lib)))
        ;;         test-libs)))
        (map snow2-library-name test-libs))

      (define (run-tests env)
        (let ((result #f))
          (guard
           (err (#t
                 (cond (verbose
                        (write (error-object-message err))
                        (newline)
                        (write (error-object-irritants err))
                        (newline)
                        ;; (raise err)
                        ))))

           ;; (let ((output
           ;;        (with-output-to-string
           ;;          (lambda () (set! result (eval '(run-tests) env))))
           ;;        ))
           ;;   ;; ... if verbose, write output?
           ;;   #t)

           (set! result (eval '(run-tests) env)))
          result))

      (let* ((test-results
              (local-packages-operation
               repositories package-metafiles

               (lambda (local-repository package-metafile package)
                 (set-snow2-repository-dirty! local-repository #f)
                 (display "calling run-tests for ")
                 (write package-metafile)
                 (display " --> ")

                 (let* ((test-libs
                         (filter
                          (lambda (lib) (memq 'test (snow2-library-use-for lib)))
                          (snow2-package-libraries package)))
                        (test-lib-names (get-test-lib-names test-libs))
                        (env (build-test-environment test-lib-names)))
                   (cond ((not env)
                          (display "test lib load failed")
                          (newline)
                          (list package 'lib-load-failed))
                         ((run-tests env)
                          (display "ok\n")
                          (list package 'ok))
                         (else
                          (display "failed\n")
                          (list package 'failed)))))

               verbose))

             (failed-tests
              (filter
               (lambda (test-result)
                 (not (eq? (cadr test-result) 'ok)))
               test-results)))

        (cond ((pair? failed-tests)
               (display "failed tests:")
               (newline)
               (for-each
                (lambda (test-result)
                  (let ((package (car test-result))
                        (result (cadr test-result)))
                    (display "    ")
                    (display (snow2-package-get-readable-name package))
                    (display ": ")
                    (write result)
                    (newline)))
                failed-tests)))))



    (define (list-replace-last lst new-elt)
      ;; what's the srfi-1 one-liner for this?
      (reverse (cons new-elt (cdr (reverse lst)))))


    (define (upload-packages-to-s3 credentials repositories
                                   package-metafiles verbose)
      ;; to avoid re-uploading indexes, use dirty flag on
      ;; repository to keep track.
      (for-each (lambda (repository)
                  (set-snow2-repository-dirty! repository #t))
                repositories)
      ;; call upload-package-to-s3 for each pacakge
      (local-packages-operation
       repositories package-metafiles
       (lambda (local-repository package-metafile package)
         (upload-package-to-s3 credentials local-repository package)
         ;; if this repository is still dirty, upload its index files
         ;; and set dirty to #f.
         (cond ((not (snow2-repository-url local-repository))
                (display "can't determine upload uri for repository.\n")
                (exit 1))
               ((snow2-repository-dirty local-repository)
                (let* ((repo-uri (snow2-repository-url local-repository))
                       (index-uri (repo-url->file-url repo-uri '("index.scm")))
                       (html-uri (repo-url->file-url repo-uri '("index.html")))
                       (css-uri (repo-url->file-url repo-uri '("index.css")))
                       ;; figure out which s3 bucket we're uploading to
                       (index-bucket (uri->bucket index-uri))
                       ;; figure out s3 path
                       (index-s3-path (uri->path-string index-uri))
                       (html-s3-path (uri->path-string html-uri))
                       (css-s3-path (uri->path-string css-uri))
                       ;; ready credentials for the intended bucket
                       (credentials
                        (if credentials credentials
                            (get-credentials-for-s3-bucket index-bucket))))

                  ;; upload index.scm
                  (conditional-put-object!
                   credentials index-bucket
                   index-s3-path
                   (local-repository->in-fs-index-filename local-repository))
                  ;; upload index.html
                  (conditional-put-object!
                   credentials index-bucket
                   html-s3-path
                   (local-repository->in-fs-html-filename local-repository))
                  ;; upload index.css
                  (conditional-put-object!
                   credentials index-bucket
                   css-s3-path
                   (local-repository->in-fs-css-filename local-repository))
                  (set-snow2-repository-dirty! local-repository #f)))))
       verbose))


    (define (import-is-needed? imported-lib-exports lib-body-symbols)
      (find (lambda (imported-lib-export)
              (member imported-lib-export lib-body-symbols))
            imported-lib-exports))


    (define (check-packages credentials repositories package-metafiles
                            verbose)
      (local-packages-operation
       repositories package-metafiles
       (lambda (local-repository package-metafile package)
         (let ((meta-data (let* ((p (open-input-file package-metafile))
                                 (meta-data (read p)))
                            (close-input-port p)
                            meta-data)))

           ;; check for any auto-generated meta-data fields in the
           ;; package's meta-data file.
           (for-each
            (lambda (unwanted-clause-name)
              (cond ((get-child-by-type meta-data unwanted-clause-name #f)
                     (display "package meta-file ")
                     (write package-metafile)
                     (display " has (")
                     (display unwanted-clause-name)
                     (display " ...).\n"))))
            (list 'size 'checksum))

           (for-each
            (lambda (lib-meta-sexp)
              (for-each
               (lambda (unwanted-clause-name)
                 (cond ((get-child-by-type
                         lib-meta-sexp unwanted-clause-name #f)
                        (display "in package meta-file ")
                        (write package-metafile)
                        (display " library ")
                        (write (cadr (get-child-by-type lib-meta-sexp 'path)))
                        (display " has (")
                        (display unwanted-clause-name)
                        (display " ...)\n"))))
               (list 'name 'depends)))
            (get-children-by-type meta-data 'library))


           (for-each
            (lambda (lib)
              (let* (;; find the .sld file in the local filesystem
                     (lib-filename (local-repository->in-fs-lib-filename
                                    local-repository lib))
                     ;; read in the .sld s-expression
                     (lib-sexp (r7rs-library-file->sexp lib-filename))
                     ;; extract the names of imported libraries from lib-sexp
                     (lib-imports
                      (r7rs-get-imported-library-names lib-sexp verbose))
                     (pkg-depends (snow2-library-depends lib))
                     (deps-unneeded
                      (lset-difference equal? pkg-depends lib-imports))
                     (lib-body-symbols
                      (r7rs-get-referenced-symbols lib-filename lib-sexp))
                     (import-decls (r7rs-get-import-decls lib-sexp))
                     )

                (cond
                 ((pair? deps-unneeded)
                  (display "library ")
                  (write (snow2-library-name lib))
                  (display " in ")
                  (write package-metafile)
                  (display " has unneeded depends: ")
                  (write deps-unneeded)
                  (newline)))

                ;; (display "-------lib-body-symbols-------\n")
                ;; (write lib-body-symbols)
                ;; (newline)

                ;; (display "-------import-decls-------\n")
                ;; (write import-decls)
                ;; (newline)

                (for-each
                 (lambda (import-decl)

                   ;; (display "  ------")
                   ;; (write import-decl)
                   ;; (display "\n")

                   (let-values (((imported-lib-name imported-lib-exports)
                                 (r7rs-get-exports-from-import-set
                                  repositories import-decl)))

                     (cond ((and
                             imported-lib-name
                             verbose
                             (not (is-system-import? imported-lib-name)))
                            (display "  in ")
                            (write (snow2-library-path lib))
                            (display ", ")
                            (write imported-lib-name)
                            (display " provides:")
                            (for-each (lambda (lib-body-symbol)
                                        (cond ((member lib-body-symbol
                                                       imported-lib-exports)
                                               (display " ")
                                               (write lib-body-symbol))))
                                      lib-body-symbols)
                            (newline)))

                     (cond ((and
                             imported-lib-name
                             (not (is-system-import? imported-lib-name))
                             (not (import-is-needed?
                                   imported-lib-exports
                                   lib-body-symbols)))
                            (display "in ")
                            (write lib-filename)
                            (display " unused import: ")
                            (write imported-lib-name)
                            (newline)

                            ;; (write imported-lib-exports)
                            ;; (newline)

                            ))))
                 import-decls)
                ))
            (snow2-package-libraries package))))
       verbose))

    ))
