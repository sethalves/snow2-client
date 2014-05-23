(define-library (seth snow2 manage)
  (export make-package-archives
          upload-packages-to-s3
          check-packages)

  (import (scheme base)
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

  (begin

    (define (make-package-archive
             repositories local-repository package verbose)
      ;; create the .tgz file that gets uploaded to a repository.
      ;; update the size and md5 sum and depends in the package meta-data
      ;; in index.scm.

      (cond
       (verbose
        (display "-- packaging ")
        (write (uri->string (snow2-package-url package)))
        (newline)))

      (let* ((repo-path (uri-path (snow2-repository-url local-repository))))

        (define (lib-file->tar-recs lib-filename)
          ;; create a tar-rec for a file
          (let* ((lib-rel-path (snow-split-filename lib-filename))
                 (lib-full-path (append repo-path lib-rel-path))
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
                     (tar-rec-name-set! (car tar-recs) lib-filename)
                     (car tar-recs))))))

        (define (lib-dir->tar-recs lib-dirname)
          ;; create a tar-rec for a directory
          (let* ((lib-rel-path (snow-split-filename lib-dirname))
                 (tar-rec
                  (make-tar-rec
                   (snow-combine-filename-parts
                    (append lib-rel-path (list "")))
                   493 ;; mode
                   0 ;; uid
                   0 ;; gid
                   ;; (exact (floor (current-second))) ;; mtime
                   (snow-file-mtime
                    (snow-combine-filename-parts
                     (append repo-path lib-rel-path)))
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
               ;; combine them
               (all-tar-recs (append dir-tar-recs file-tar-recs))
               ;; figure out the name of the tgz file within the local repo
               (package-url (snow2-package-url package))
               (package-filename
                (if (and package-url (pair? (uri-path package-url)))
                    (last (uri-path package-url))
                    "unknown.tgz"))
               (local-package-path (append repo-path (list package-filename)))
               (local-package-filename
                (snow-combine-filename-parts local-package-path)))

          ;; get a list of libraries this package depends on and
          ;; set library names.
          (for-each (lambda (lib lib-sexp)

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
                         ((not (equal?
                                (sort (snow2-library-depends lib) lib-name<?)
                                (sort deps lib-name<?)))
                          (set-snow2-package-dirty! package #t)
                          (set-snow2-library-depends! lib deps)
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
          (cond ((or (snow-file-exists? local-package-filename)
                     (snow-file-symbolic-link? local-package-filename))
                 (snow-delete-file local-package-filename)))

          ;; save out the new .tgz file
          (let* ((tar-data (tar-pack-u8vector all-tar-recs))
                 (tgz-data (gzip-u8vector tar-data))
                 (out-p (open-binary-output-file local-package-filename)))

            (write-bytevector tgz-data out-p)
            (close-output-port out-p)

            (let ((local-package-md5
                   (bytes->hex-string
                    (filename->md5 local-package-filename)))
                  (local-package-size (snow-file-size local-package-filename)))

              (display "  size=")
              (write local-package-size)
              (display " md5=")
              (write local-package-md5)
              (newline)

              (cond ((and (number? (snow2-package-size package))
                          (not (= (snow2-package-size package)
                                  local-package-size)))
                     (set-snow2-package-size! package local-package-size)
                     (set-snow2-package-dirty! package #t)
                     (display "setting package dirty due to size\n")))
              (cond ((not (equal? (snow2-package-checksum package)
                                  `(md5 ,local-package-md5)))
                     (set-snow2-package-checksum!
                      package `(md5 ,local-package-md5))
                     (set-snow2-package-dirty! package #t)
                     (display "setting package dirty due to md5\n")))

              ;; (set-snow2-package-dirty! package #t)
              ;; (set-snow2-repository-dirty! local-repository #t)
              )))))


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
                      (raise err)
                      ))
                (put-object! credentials bucket s3-path local-p
                             #f ;; (snow-file-size local-filename)
                             "application/octet-stream"
                             'public-read))
               (close-input-port local-p)))))


    (define (upload-package-to-s3 credentials local-repository package)
      (let* ((url (snow2-package-url package))
             (bucket (uri->bucket url))
             (s3-path (uri->path-string url))
             (credentials (if credentials credentials
                              (get-credentials-for-s3-bucket bucket))))
        (conditional-put-object!
         credentials bucket s3-path
         (local-repository->in-fs-tgz-filename local-repository package))))


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
      (let* ((repo-path (uri-path (snow2-repository-url local-repository)))
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
             (let* ((repo-uri (snow2-repository-url local-repository))
                    (repo-path (uri-path repo-uri)))
               (snow-combine-filename-parts
                (append repo-path
                        (list "packages" (string-append
                                          package-metafile
                                          ".package"))))))))



    (define (resolve-package-metafiles local-repository package-metafiles)
      ;; call resolve-package-metafile for each of package-metafiles
      (map (lambda (package-file)
             (resolve-package-metafile local-repository package-file))
           package-metafiles))


    (define (find-implied-local-package-metafiles local-repository)
      ;; look at the current working directory and see if
      ;; the user intends to operate on a specific package in the
      ;; repository, or on all of them.
      (let* ((repo-path (uri-path (snow2-repository-url local-repository)))
             (cwd (get-environment-variable "PWD"))
             (cwd-parts (snow-split-filename cwd))
             (cwd-parts-len (length cwd-parts))
             (cwd-from-end
              (lambda (n)
                (and (> cwd-parts-len n)
                     (list-ref cwd-parts (- cwd-parts-len n))))))
        (cond
         ;; are we in the tests directory?
         ((equal? "tests" (cwd-from-end 1))
          ;; (display "in test directory...\n")
          (all-package-metafiles local-repository))
         ;; are we in a specific test directory?
         ((and (equal? "tests" (cwd-from-end 2))
               (let ((package-filename
                      (snow-combine-filename-parts
                       (append
                        repo-path
                        (list "packages")
                        (list (string-append (cwd-from-end 1) ".package"))))))
                 (if (snow-file-exists? package-filename)
                     package-filename
                     #f))) =>
                     (lambda (package-filename)
                       ;; (display "in specific test directory...\n")
                       (list package-filename)))
         ;; are we in the packages directory?
         ((equal? "packages" (cwd-from-end 1))
          ;; (display "in packages directory...\n")
          (all-package-metafiles local-repository))
         (else
          ;; (display "don't know...\n")
          (all-package-metafiles local-repository)))))


    (define (local-repository-operation repositories op verbose)
      ;; decide which local repository is intended.
      ;; call (op local-repository)
      (let ((repositories (filter snow2-repository-local repositories)))
        (let ((repository
               (cond ((> (length repositories) 1)
                      ;; (error "multiple local repositories given.\n")
                      ;; (find-implied-local-repository)
                      (let ((repo (last repositories)))
                        (display "multiple local repositories given, using ")
                        (write (uri->string (snow2-repository-url repo)))
                        (newline)
                        repo))
                     ((= (length repositories) 1)
                      (car repositories))
                     (else (find-implied-local-repository)))))
          ;; (display "operating on repository: ")
          ;; (write (uri-path (snow2-repository-url repository)))
          ;; (newline)
          (cond (repository
                 (sanity-check-repository repository)
                 (op repository)
                 (sanity-check-repository repository)
                 ;; update index.scm if the repository is "dirty"
                 (cond ((snow2-repository-dirty repository)
                        (let* ((index-scm-filename
                                (local-repository->in-fs-index-filename
                                 repository))
                               (p (open-output-file index-scm-filename)))
                          (cond (verbose
                                 (display "rewriting ")
                                 (write index-scm-filename)
                                 (newline)))
                          (snow-pretty-print (repository->sexp repository) p)
                          (close-output-port p)))))
                (else
                 (error
                  "Unable to determine which repository to operate on."))))))


    (define (local-packages-operation repositories package-metafiles op verbose)
      ;; decide which local repository is intended.
      ;; call (op local-repo package) for each package-file.
      ;; return a list of results.
      (local-repository-operation
       repositories
       (lambda (local-repository)
         (let* ((package-metafiles
                 (cond ((pair? package-metafiles) package-metafiles)
                       (else (find-implied-local-package-metafiles
                              local-repository))))
                (package-metafiles
                 (resolve-package-metafiles local-repository package-metafiles))
                (result
                 (map
                  (lambda (package-metafile)

                    ;; (display "operating on package: ")
                    ;; (write package-metafile)
                    ;; (newline)

                    (let* ((package
                            ;; (package-from-filename package-metafile)
                            (refresh-package-from-filename
                             local-repository package-metafile verbose))
                           (result (op local-repository
                                       package-metafile
                                       package)))

                      ;; if the package changed, we'll need to rewrite index.scm
                      (cond ((snow2-package-dirty package)
                             (set-snow2-repository-dirty! local-repository #t)
                             (if verbose
                                 (display "set repo dirty because package is\n")
                             )))
                      result))
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
          local-repository package verbose))
       verbose))


    (define (list-replace-last lst new-elt)
      ;; what's the srfi-1 one-liner for this?
      (reverse (cons new-elt (cdr (reverse lst)))))


    (define (upload-packages-to-s3 credentials repositories
                                   package-metafiles verbose)
      (local-packages-operation
       repositories package-metafiles
       (lambda (local-repository package-metafile package)
         (upload-package-to-s3 credentials local-repository package)
         ;; assume that index.scm lives next to the tgz file in the
         ;; s3 repository.
         (let* ((package-uri (snow2-package-url package))
                ;; take the package's uri and replace the filename
                ;; with index.scm
                (package-path (uri-path package-uri))
                (index-path (list-replace-last package-path "index.scm"))
                (index-uri (update-uri package-uri 'path index-path))
                ;; figure out which s3 bucket we're uploading to
                (index-bucket (uri->bucket index-uri))
                ;; figure out s3 path
                (index-s3-path (uri->path-string index-uri))
                ;; ready credentials for the intended bucket
                (credentials
                 (if credentials credentials
                     (get-credentials-for-s3-bucket index-bucket))))
           (conditional-put-object!
            credentials index-bucket
            index-s3-path
            (local-repository->in-fs-index-filename local-repository))))
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
