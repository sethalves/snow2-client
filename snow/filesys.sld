(define-library (snow filesys)
  (export snow-directory-files
          snow-file-exists?
          snow-file-directory?
          snow-delete-file
          snow-rename-file
          snow-create-directory
          snow-delete-directory
          snow-filename-extension
          snow-filename-strip-extension
          snow-filename-directory
          snow-filename-strip-directory
          snow-filename-strip-trailing-directory-separator
          snow-make-filename
          ;; snow-make-temp-filename
          snow-directory-subfiles
          snow-create-directory-recursive)
  (import (scheme base)
          (scheme file)
          (snow bytevector)
          (snow random))
  (cond-expand
   (chibi
    (import (srfi 1) (chibi filesystem)))
   (chicken
    (import (chicken))
    (import (posix))
    ;; (import (posix-extras))
    ;; (import (directory-utils))
    )
   (gauche
    (import (gauche fileutil) (file util)))
   (sagittarius
    (import (sagittarius) (srfi 1) (util file)))
   (else))

  (begin


;;;============================================================================

;;; System dependencies.

    (cond-expand

     (bigloo

      (define (snow-directory-files dir)
        (reverse (directory->list dir)))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (if (not (make-directory dir))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (delete-directory dir)))

     (chez

      (define (snow-directory-files dir)

        (define (read-line port)
          (let ((c (read-char port)))
            (if (char? c)
                (let loop ((lst (list c)))
                  (let ((c (read-char port)))
                    (if (and (char? c) (not (char=? c #\newline)))
                        (loop (cons c lst))
                        (list->string (reverse lst)))))
                c)))

        (define (read-lines port)
          (let loop ((lst '()))
            (let ((s (read-line port)))
              (if (string? s)
                  (loop (cons s lst))
                  (reverse lst)))))

        (let* ((p (process (string-append "ls -a \"" dir "\" 2> /dev/null")))
               (i (car p))
               (o (cadr p))
               (files (read-lines i)))
          (close-input-port i)
          (close-output-port o)
          (remove "."
                  (remove ".."
                          files))))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-exists? (string-append filename "/.")))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (if (not (= 0 (system (string-append "mv \"" orig-filename "\" \"" new-filename "\" 2> /dev/null"))))
            (snow-raise "could not rename file")))

      (define (snow-create-directory dir)
        (if (not (= 0 (system (string-append "mkdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (if (not (= 0 (system (string-append "rmdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not delete directory"))))


     (chibi

      (define (snow-directory-files dir)
        (filter (lambda (ent)
                  (not (or (equal? ent ".")
                           (equal? ent ".."))))
                (directory-files dir)))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (create-directory* dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))


     (chicken

      (define (snow-directory-files dir)
        (directory dir #t))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (create-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))

     (gambit

      (define (snow-directory-files dir)
        (directory-files (list path: dir ignore-hidden: 'dot-and-dot-dot)))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (eq? (file-type filename) 'directory))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (create-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))

     (gauche

      (define (snow-directory-files dir)
        (let loop ((filenames (directory-list dir))
                   (result (list)))
          (cond ((null? filenames) (reverse result))
                ((equal? (car filenames) ".") (loop (cdr filenames) result))
                ((equal? (car filenames) "..") (loop (cdr filenames) result))
                (else (loop (cdr filenames) (cons (car filenames) result))))))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        ;; (file-is-directory? filename)
        (eq? (file-type filename) 'directory))

      (define (snow-delete-file filename)
        ;; (sys-unlink filename)
        (remove-files (list filename)))

      (define (snow-rename-file orig-filename new-filename)
        ;; (sys-rename orig-filename new-filename)
        (move-file orig-filename new-filename))

      (define (snow-create-directory dir)
        ;; (sys-mkdir dir #o777)
        (make-directory* dir))

      (define (snow-delete-directory dir)
        ;; (sys-rmdir dir)
        (remove-directory* dir)))

     (guile

      (define (snow-directory-files dir)
        (let ((d (opendir dir)))
          (let loop ((lst '()))
            (let ((entry (readdir d)))
              (if (eof-object? entry)
                  (begin
                    (closedir d)
                    (delete "."
                            (delete ".."
                                    lst)))
                  (loop (cons entry lst)))))))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-exists? (string-append filename "/.")))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (mkdir dir))

      (define (snow-delete-directory dir)
        (rmdir dir)))

     (kawa

      (define (snow-directory-files dir)
        (directory-files dir))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (if (file-exists? dir)
            (snow-raise "could not create directory")
            (create-directory dir)))

      (define (snow-delete-directory dir)
        (if (not (file-exists? dir))
            (snow-raise "could not delete directory")
            (delete-file dir))))

     (larceny

      (define (snow-directory-files dir)
        (filter (lambda (ent)
                  (not (or (equal? ent ".")
                           (equal? ent ".."))))
                (list-directory dir)))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-exists? (string-append filename "/.")))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (if (not (= 0 (system (string-append "mkdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (if (not (= 0 (system (string-append "rmdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not delete directory"))))

     (mit

      (define (snow-directory-files dir)
        (delete "."
                (delete ".."
                        (map file-namestring
                             (directory-read (string-append dir "/"))))))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (make-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))

     (mzscheme

      (define (snow-directory-files dir)
        (map path->string (directory-list dir)))

      (define (snow-file-exists? filename)
        (or (file-exists? filename)
            (directory-exists? filename)))

      (define (snow-file-directory? filename)
        (and (not (file-exists? filename))
             (directory-exists? filename)))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file-or-directory orig-filename new-filename))

      (define (snow-create-directory dir)
        (make-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))


     (sagittarius

      (define (snow-directory-files dir)
        (filter (lambda (ent)
                  (not (or (equal? ent ".")
                           (equal? ent ".."))))
                (read-directory dir)))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (create-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))



     (scheme48

      (define (snow-directory-files dir)
        (list-directory dir))

      (define (snow-file-exists? filename)
        (accessible? filename (access-mode read)))

      (define (snow-file-directory? filename)
        (snow-file-exists? (string-append filename "/.")))

      (define (snow-delete-file filename)
        (unlink filename))

      (define (snow-rename-file orig-filename new-filename)
        (if (not (= 0 (system (string-append "mv \"" orig-filename "\" \"" new-filename "\" 2> /dev/null"))))
            (snow-raise "could not rename file")))

      (define (snow-create-directory dir)
        (if (not (= 0 (system (string-append "mkdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (if (not (= 0 (system (string-append "rmdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not delete directory"))))

     (scm

      (require 'common-list-functions)
      (require 'filename)

      (define (snow-directory-files dir)
        (call-with-tmpnam
         (lambda (tmp)
           (if (and (= 0 (system (string-append "ls -a \"" dir "\" > " tmp)))
                    (file-exists? tmp))
               (call-with-input-file tmp
                 (lambda (port)
                   (let loop ((rev-filenames '()))
                     (let ((x (read-line port)))
                       (if (not (eof-object? x))
                           (loop (cons x rev-filenames))
                           (delete "."
                                   (delete ".."
                                           (reverse rev-filenames))))))))
               (snow-raise "could not list directory")))))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-exists? (string-append filename "/.")))

      (define (snow-delete-file filename)
        (if (not (= 0 (system (string-append "rm -f \"" filename "\" 2> /dev/null"))))
            (snow-raise "could not delete file")))

      (define (snow-rename-file orig-filename new-filename)
        (if (not (= 0 (system (string-append "mv \"" orig-filename "\" \"" new-filename "\" 2> /dev/null"))))
            (snow-raise "could not rename file")))

      (define (snow-create-directory dir)
        (if (not (= 0 (system (string-append "mkdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (if (not (= 0 (system (string-append "rmdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not delete directory"))))

     (scsh

      (define (snow-directory-files dir)
        (directory-files dir #t))

      (define (snow-file-exists? filename)
        (file-exists? filename))

      (define (snow-file-directory? filename)
        (file-directory? filename))

      (define (snow-delete-file filename)
        (delete-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (create-directory dir))

      (define (snow-delete-directory dir)
        (delete-directory dir)))

     (sisc

      (import file-manipulation) 

      (define^ (snow-directory-files dir)
        (directory-list dir))

      (define^ (snow-file-exists? filename)
        (file-exists? filename))

      (define^ (snow-file-directory? filename)
        (file-is-directory? filename))

      (define^ (snow-delete-file filename)
        (file-delete! filename))

      (define^ (snow-rename-file orig-filename new-filename)
        (if (not (file-rename! orig-filename new-filename))
            (snow-raise "could not rename file")))

      (define^ (snow-create-directory dir)
        (if (not (make-directory! dir))
            (snow-raise "could not create directory")))

      (define^ (snow-delete-directory dir)
        (if (not (file-delete! dir))
            (snow-raise "could not delete directory"))))

     (stklos

      ;; Could be improved for STklos 0.82

      (define (snow-directory-files dir)
        (delete "."
                (delete ".."
                        (exec-list (string-append "ls -a \"" dir "\"")))))

      (define (snow-file-exists? filename)
        (or (file-exists? filename)
            (file-is-directory? filename)))

      (define (snow-file-directory? filename)
        (file-is-directory? filename))

      (define (snow-delete-file filename)
        (remove-file filename))

      (define (snow-rename-file orig-filename new-filename)
        (rename-file orig-filename new-filename))

      (define (snow-create-directory dir)
        (if (not (= 0 (system (string-append "mkdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not create directory")))

      (define (snow-delete-directory dir)
        (if (not (= 0 (system (string-append "rmdir \"" dir "\" 2> /dev/null"))))
            (snow-raise "could not delete directory")))))

;;;----------------------------------------------------------------------------

    (define (extension-separator) #\.)
    (define (directory-separator) #\/)

    (define (snow-filename-extension-index filename)
      (let ((end (string-length filename)))
        (let loop ((i (- end 1)))
          (if (< i 0)
              end
              (let ((c (string-ref filename i)))
                (cond ((char=? c (extension-separator))
                       i)
                      ((char=? c (directory-separator))
                       end)
                      (else
                       (loop (- i 1)))))))))

    (define (snow-filename-extension filename)
      (substring filename
                 (snow-filename-extension-index filename)
                 (string-length filename)))

    (define (snow-filename-strip-extension filename)
      (substring filename
                 0
                 (snow-filename-extension-index filename)))

    (define (snow-filename-directory-index filename)
      (let ((end (string-length filename)))
        (let loop ((i (- end 1)))
          (if (< i 0)
              0
              (let ((c (string-ref filename i)))
                (cond ((char=? c (directory-separator))
                       (+ i 1))
                      (else
                       (loop (- i 1)))))))))

    (define (snow-filename-directory filename)
      (substring filename
                 0
                 (snow-filename-directory-index filename)))

    (define (snow-filename-strip-directory filename)
      (substring filename
                 (snow-filename-directory-index filename)
                 (string-length filename)))

    (define (snow-filename-strip-trailing-directory-separator filename)
      (let ((end (string-length filename)))
        (if (and (< 0 end)
                 (char=? (string-ref filename (- end 1)) (directory-separator)))
            (substring filename 0 (- end 1))
            filename)))

    (define (snow-make-filename part1 . parts)
      (let loop ((filename part1) (lst parts))
        (if (pair? lst)
            (loop (string-append filename
                                 (string (directory-separator))
                                 (car lst))
                  (cdr lst))
            filename)))

    ;; (define (snow-make-temp-filename)
    ;;   (let loop ()
    ;;     (let ((filename (snow-u8vector->hex-string (make-random-u8vector 6))))
    ;;       (if (snow-file-exists? filename)
    ;;           (loop)
    ;;           filename))))

    ;; (define* (snow-directory-subfiles filename (types '(regular directory)))
    (define (snow-directory-subfiles filename . maybe-types)
      (let ((types (if (null? maybe-types)
                       '(regular directory)
                       (car maybe-types))))

        (define (list-file filename rev-files)
          (let* ((t
                  (if (snow-file-directory? filename)
                      'directory
                      'regular))
                 (rf
                  (if (memq t types)
                      (cons filename rev-files)
                      rev-files)))
            (if (eq? t 'directory)
                (list-dir filename rf)
                rf)))

        (define (list-dir dir rev-files)
          (let loop ((lst (snow-directory-files dir))
                     (rev-files rev-files))
            (if (pair? lst)
                (let* ((name
                        (car lst))
                       (filename
                        (snow-make-filename dir name)))
                  (loop (cdr lst)
                        (list-file filename rev-files)))
                rev-files)))

        (reverse (list-file filename '()))))

    (define (snow-create-directory-recursive dir)
      (let ((d (snow-filename-strip-trailing-directory-separator dir)))
        (if (not (string=? d dir))
            (snow-create-directory-recursive d)
            (if (not (snow-file-exists? dir))
                (let ((p (snow-filename-directory dir)))
                  (if (not (string=? p dir))
                      (begin
                        (snow-create-directory-recursive p)
                        (snow-create-directory dir))))))))

;;;============================================================================


    ))
