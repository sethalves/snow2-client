(define-library (seth snow2 manage)
  (export make-package-archives
          upload-packages-to-s3
          check-packages)

  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme file)
          (scheme time)
          (scheme process-context)
          )
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any
                        fold last lset-difference delete-duplicates
                        drop-right)))
   (else (import (srfi 1))))
  (import (snow snowlib)
          (snow bytevector)
          (snow tar)
          (snow zlib)
          (snow filesys)
          (snow srfi-13-strings)
          (snow extio)
          (snow srfi-29-format)
          (seth uri)
          (seth crypt md5)
          (seth aws common)
          (seth aws s3)
          (seth snow2 types)
          (seth snow2 utils)
          (seth snow2 r7rs-library))

  (begin

    (define (make-package-archive local-repository package)
      ;; create the .tgz file that gets uploaded to a repository.
      ;; update the size and md5 sum in the package meta-data
      (let* ((repo-path (uri-path (snow2-repository-url local-repository))))

        (define (lib-file->tar-recs lib-filename)
          ;; create a tar-rec for a file
          (let* ((lib-rel-path (snow-split-filename lib-filename))
                 (lib-full-path (append repo-path lib-rel-path))
                 (lib-full-filename (snow-combine-filename-parts lib-full-path))
                 (tar-recs (tar-read-file lib-full-filename)))
            (cond ((not (= (length tar-recs) 1))
                   (error "unexpected tar-rec count" lib-filename tar-recs)))
            (tar-rec-name-set! (car tar-recs) lib-filename)
            (car tar-recs)))

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
                   (exact (floor (current-second))) ;; mtime
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
               ;; get a list of filenames for all libraries in the package
               (manifest (fold append '() (map get-library-manifest libraries)))
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
               (package-filename (last (uri-path package-url)))
               (local-package-path (append repo-path (list package-filename)))
               (local-package-filename
                (snow-combine-filename-parts local-package-path)))
          (display "writing ")
          (display local-package-path)
          (newline)

          (cond ((or (snow-file-exists? local-package-filename)
                     (snow-file-symbolic-link? local-package-filename))
                 (snow-delete-file local-package-filename)))

          (let* ((tar-data (tar-pack-u8vector all-tar-recs))
                 (tgz-data (gzip-u8vector tar-data))
                 (out-p (open-binary-output-file local-package-filename)))

            (write-bytevector tgz-data out-p)
            (close-output-port out-p)

            (let ((local-package-md5
                   (bytes->hex-string
                    (filename->md5 local-package-filename)))
                  (local-package-size (snow-file-size local-package-filename)))
              (display (format "  size=~a md5=~a\n"
                               local-package-size local-package-md5))

              (set-snow2-package-size! package local-package-size)
              (set-snow2-package-checksum!
               package `(md5 ,local-package-md5))
              (set-snow2-package-dirty! package #t))))))


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
               (display "]")
               (newline)
               (snow-with-exception-catcher
                (lambda (err)
                  (snow-display-error err)
                  (exit 1))
                (lambda ()
                  (put-object! credentials bucket s3-path local-p
                               #f ;; (snow-file-size local-filename)
                               "application/octet-stream"
                               'public-read)
                  ))
               (close-input-port local-p)))))


    (define (upload-package-to-s3 credentials local-repository package)
      (let* ((repo-path (uri-path (snow2-repository-url local-repository)))
             (url (snow2-package-url package))
             (local-package-path (reverse
                                  (cons (last (uri-path url))
                                        (reverse repo-path))))
             (local-package-filename (snow-combine-filename-parts
                                      local-package-path))
             (local-package-port
              (open-binary-input-file local-package-filename))
             (bucket (uri->bucket url))
             (s3-path (uri->path-string url))
             (credentials (if credentials credentials
                              (get-credentials-for-s3-bucket bucket))))
        (conditional-put-object!
         credentials bucket s3-path local-package-filename)
        (close-input-port local-package-port)))


    (define (find-implied-local-repository)
      ;; figure out which repository (if any) the user's CWD was within when
      ;; this program was run.
      (or
       (get-repository
        (uri-reference (snow-combine-filename-parts '("."))))
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".."))))
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".."))))
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".." ".."))))
       (get-repository
        (uri-reference (snow-combine-filename-parts '(".." ".." ".." ".."))))))


    (define (all-package-files local-repository)
      ;; return a list of package-file filenames for the given
      ;; local repository
      (let* ((repo-path (uri-path (snow2-repository-url local-repository)))
             (repo-dirname (snow-combine-filename-parts repo-path))
             (packages-dirname (snow-make-filename repo-dirname "packages")))
        (map (lambda (package-filename)
               (snow-make-filename packages-dirname package-filename))
             (snow-directory-files packages-dirname))))


    (define (resolve-package-file local-repository package-file)
      ;; package-files may be a path (absolute or relative) to
      ;; a repo/packages/NAME.package file, or they may be
      ;; just NAME.  either way, return the former.
      (cond ((string-suffix? ".package" package-file) package-file)
            (else
             (let* ((repo-uri (snow2-repository-url local-repository))
                    (repo-path (uri-path repo-uri)))
               (snow-combine-filename-parts
                (append repo-path
                        (list "packages"
                              (string-append package-file ".package"))))))))



    (define (resolve-package-files local-repository package-files)
      ;; call resolve-package-file for each of package-files
      (map (lambda (package-file)
             (resolve-package-file local-repository package-file))
           package-files))


    (define (find-implied-local-package-files local-repository)
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
          (all-package-files local-repository))
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
          (all-package-files local-repository))
         (else
          ;; (display "don't know...\n")
          (all-package-files local-repository)))))


    (define (local-repository-operation repositories op)
      ;; decide which local repository is intended.
      ;; call (op local-repository)
      (let ((repositories (filter snow2-repository-local repositories)))
        (let ((repository
               (cond ((> (length repositories) 1)
                      (error "multiple local repositories given.\n"))
                     ((= (length repositories) 1)
                      (car repositories))
                     (else (find-implied-local-repository)))))
          ;; (display "operating on repository: ")
          ;; (write (uri-path (snow2-repository-url repository)))
          ;; (newline)
          (cond (repository
                 (op repository))
                (else
                 (error
                  "Unable to determine which repository to operate on."))))))


    (define (local-packages-operation repositories package-files op)
      ;; decide which local repository is intended.
      ;; call (op local-repo package) for each package-file.
      ;; return a list of results.
      (local-repository-operation
       repositories
       (lambda (local-repository)
         (let* ((package-files
                 (cond ((pair? package-files) package-files)
                       (else (find-implied-local-package-files
                              local-repository))))
                (repo-path (uri-path (snow2-repository-url local-repository)))
                (index-filename (snow-combine-filename-parts
                                 (append repo-path (list "index.scm"))))
                (package-files
                 (resolve-package-files local-repository package-files))
                (result
                 (map
                  (lambda (package-metafile)

                    ;; (display "operating on package: ")
                    ;; (write package-metafile)
                    ;; (newline)

                    (let* ((package
                            ;; (package-from-filename package-metafile)
                            (refresh-package-from-filename
                             local-repository package-metafile))
                           (result (op local-repository
                                       package-metafile
                                       package)))
                      ;; rewrite package meta-file to sync any changes
                      (cond ((snow2-package-dirty package)
                             (let ((p (open-output-file package-metafile)))
                               (snow-pretty-print (package->sexp package) p)
                               (close-output-port p)
                               (set-snow2-repository-dirty!
                                local-repository #t))))
                      result))
                  package-files)))
           (cond ((snow2-repository-dirty local-repository)
                  (let ((p (open-output-file index-filename)))
                    (snow-pretty-print (repository->sexp local-repository) p)
                    (close-output-port p))))
           result))))


    (define (make-package-archives repositories package-files)
      ;; call make-package-archive for each of packages-files
      (local-packages-operation
       repositories package-files
       (lambda (local-repository package-metafile package)
         (refresh-package-from-filename local-repository package-metafile)
         (make-package-archive local-repository package))))


    (define (list-replace-last lst new-elt)
      ;; what's the srfi-1 one-liner for this?
      (reverse (cons new-elt (cdr (reverse lst)))))


    (define (upload-packages-to-s3 credentials repositories package-files)
      (local-packages-operation
       repositories package-files
       (lambda (local-repository package-metafile package)
         (upload-package-to-s3 credentials local-repository package)
         (let* ((package-uri (snow2-package-url package))
                (package-path (uri-path package-uri))
                (index-path (list-replace-last package-path "index.scm"))
                (index-uri (update-uri package-uri 'path index-path))
                (index-bucket (uri->bucket index-uri))
                (index-s3-path (uri->path-string index-uri))
                (repo-path (uri-path (snow2-repository-url local-repository)))
                (index-filename (snow-combine-filename-parts
                                 (append repo-path (list "index.scm"))))
                (credentials (if credentials credentials
                                 (get-credentials-for-s3-bucket index-bucket)))
                )
           ;; (display "index-filename=")
           ;; (write index-filename)
           ;; (newline)
           ;; (display "index-uri=")
           ;; (write (uri->string index-uri))
           ;; (newline)
           ;; (display "index-bucket=")
           ;; (write index-bucket)
           ;; (newline)
           ;; (display "index-s3-path=")
           ;; (write index-s3-path)
           ;; (newline)

           (conditional-put-object! credentials index-bucket
                                    index-s3-path index-filename)
           )
         )))


    (define (check-packages credentials repositories package-files)
      (local-packages-operation
       repositories package-files
       (lambda (local-repository package-metafile package)
         (let ((repo-path (uri-path (snow2-repository-url local-repository))))
           (map
            (lambda (lib)
              (let* ((lib-filename (snow2-library-path lib))
                     (lib-path
                      (append repo-path (snow-split-filename lib-filename)))
                     (lib-filename (snow-combine-filename-parts lib-path))
                     (lib-imports (r7rs-get-library-imports lib-filename))
                     (pkg-depends (snow2-library-depends lib))
                     (deps-missing
                      (lset-difference equal? lib-imports pkg-depends))
                     (deps-unneeded
                      (lset-difference equal? pkg-depends lib-imports))
                     )

                ;; (display "------lib imports----------\n")
                ;; (write lib-imports)
                ;; (newline)
                ;; (display "------pkg-depends----------\n")
                ;; (write pkg-depends)
                ;; (newline)

                (cond
                 ((pair? deps-missing)
                  (display "library ")
                  (write (snow2-library-name lib))
                  (display " in ")
                  (write package-metafile)
                  (display " is missing depends: ")
                  (write deps-missing)
                  (newline)))

                (cond
                 ((pair? deps-unneeded)
                  (display "library ")
                  (write (snow2-library-name lib))
                  (display " in ")
                  (write package-metafile)
                  (display " has unneeded depends: ")
                  (write deps-unneeded)
                  (newline)))

                ))
            (snow2-package-libraries package))))))

    ))
