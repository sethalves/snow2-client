;;;============================================================================

;;; File: "tar.scm", Time-stamp: <2007-04-05 00:54:26 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to pack and unpack tar files.

;; http://ktakashi.github.io/sagittarius-ref.html#archive


(define-library (snow tar)
  (export make-tar-condition
          tar-condition?
          tar-condition-msg
          tar-pack-genport
          tar-pack-file
          tar-pack-u8vector
          tar-unpack-genport
          tar-unpack-file
          tar-unpack-u8vector
          tar-read-file

          make-tar-rec tar-rec?
          tar-rec-name tar-rec-name-set!
          tar-rec-mode tar-rec-mode-set!
          tar-rec-uid tar-rec-uid-set!
          tar-rec-gid tar-rec-gid-set!
          tar-rec-mtime tar-rec-mtime-set!
          tar-rec-type tar-rec-type-set!
          tar-rec-linkname tar-rec-linkname-set!
          tar-rec-uname tar-rec-uname-set!
          tar-rec-gname tar-rec-gname-set!
          tar-rec-devmajor tar-rec-devmajor-set!
          tar-rec-devminor tar-rec-devminor-set!
          tar-rec-atime tar-rec-atime-set!
          tar-rec-ctime tar-rec-ctime-set!
          tar-rec-content tar-rec-content-set!
          )
  (import (scheme base)
          (scheme write) ;; XXX
          (scheme time)
          (snow snowlib)
          (snow bytevector)
          (snow srfi-60-integers-as-bits)
          (snow bignum)
          (snow genport)
          (snow filesys)
          )
  (begin

    (define-record-type <tar-rec>
      (make-tar-rec name mode uid gid mtime type linkname uname gname devmajor
                    devminor atime ctime content)
      tar-rec?
      (name tar-rec-name tar-rec-name-set!)
      (mode tar-rec-mode tar-rec-mode-set!)
      (uid tar-rec-uid tar-rec-uid-set!)
      (gid tar-rec-gid tar-rec-gid-set!)
      (mtime tar-rec-mtime tar-rec-mtime-set!)
      (type tar-rec-type tar-rec-type-set!)
      (linkname tar-rec-linkname tar-rec-linkname-set!)
      (uname tar-rec-uname tar-rec-uname-set!)
      (gname tar-rec-gname tar-rec-gname-set!)
      (devmajor tar-rec-devmajor tar-rec-devmajor-set!)
      (devminor tar-rec-devminor tar-rec-devminor-set!)
      (atime tar-rec-atime tar-rec-atime-set!)
      (ctime tar-rec-ctime tar-rec-ctime-set!)
      (content tar-rec-content tar-rec-content-set!))

;;;============================================================================

;;; System dependencies.

    (cond-expand

     (else

      (define (make-tar-condition msg)
        (make-snow-cond '(tar-condition)
                        (vector msg)))

      (define (tar-condition? obj)
        (and (snow-cond? obj)
             (memq 'tar-condition (snow-cond-type obj))))

      (define (tar-condition-msg cnd)
        (vector-ref (snow-cond-fields cnd) 0))))

;;;----------------------------------------------------------------------------

    (define (header-size) 512)

;;;----------------------------------------------------------------------------

    ;; Packing tar files.

    (define (tar-pack-genport tar-rec-list genport-out)

      (define tar-format 'gnu) ;; can be gnu, ustar or v7
      ;;  (define tar-format 'ustar)
      ;;  (define tar-format 'v7)

      (define use-blanks #f)

      ;; Error handling.

      (define (tar-field-overflow)
        (make-tar-condition "tar field overflow"))

      (define (tar-illegal-field)
        (make-tar-condition "tar illegal field"))

      (define (write-pad n)
        (genport-write-subu8vector
         (make-bytevector n 0)
         0
         n
         genport-out)
        #f)

      (define (write-header tr)
        (let ((header (make-bytevector (header-size) 0)))

          (define (string-field str max-length offset)
            (let ((len (string-length str)))
              (if (> len max-length)
                  (tar-field-overflow)
                  (let loop ((i 0))
                    (if (< i len)
                        (begin
                          (bytevector-u8-set!
                           header
                           (+ offset i)
                           (char->integer (string-ref str i)))
                          (loop (+ i 1)))
                        #f)))))

          (define (byte-field byte offset)
            (bytevector-u8-set! header offset byte)
            #f)

          (define (octal-field-aux n-str len offset)
            (let ((n-str-len (string-length n-str)))
              (if (> n-str-len len)
                  ;; (tar-field-overflow)
                  (error "overflow" n-str n-str-len len)
                  (let ((pad (- len n-str-len)))
                    (let loop ((i 0))
                      (if (< i pad)
                          (begin
                            (bytevector-u8-set! header
                                                (+ offset i)
                                                (if use-blanks 32 48))
                            (loop (+ i 1)))))
                    (bytevector-copy-partial!
                     (string->latin-1 n-str)
                     0
                     n-str-len
                     header
                     (+ offset pad))
                    #f))))

          (define (octal-field0 n-str len offset)
            (if use-blanks
                (bytevector-u8-set! header (+ offset (- len 1)) 32))
            (octal-field-aux n-str (- len 1) offset))

          (define (octal-field1-bignum n len offset)
            (octal-field0 (bignum->string n 8) len offset))

          (define (octal-field1 n len offset)
            (octal-field0 (number->string n 8) len offset))

          (define (octal-field2 n len offset)
            (if use-blanks
                (octal-field1 n (- len 1) offset)
                (octal-field1 n len offset)))

          (define (octal-field3 n len offset)
            (bytevector-u8-set! header (+ offset (- len 1)) 0)
            (octal-field-aux (number->string n 8) (- len 1) offset))

          (define (checksum)
            (let loop ((i 0))
              (if (< i 8)
                  (begin
                    (bytevector-u8-set! header (+ 148 i) 32)
                    (loop (+ i 1)))))
            (let loop ((sum 0) (i 0))
              (if (< i (header-size))
                  (loop (+ sum (bytevector-u8-ref header i))
                        (+ i 1))
                  sum)))

          (let* ((name     (tar-rec-name tr))
                 (mode     (tar-rec-mode tr))
                 (uid      (tar-rec-uid tr))
                 (gid      (tar-rec-gid tr))
                 (mtime    (tar-rec-mtime tr))
                 (type     (tar-rec-type tr))
                 (linkname (tar-rec-linkname tr))
                 (uname    (tar-rec-uname tr))
                 (gname    (tar-rec-gname tr))
                 (devmajor (tar-rec-devmajor tr))
                 (devminor (tar-rec-devminor tr))
                 (atime    (tar-rec-atime tr))
                 (ctime    (tar-rec-ctime tr))
                 (content  (tar-rec-content tr))
                 (size     (fixnum->bignum (bytevector-length content))))
            (or (string-field
                 (if (eq? tar-format 'gnu)
                     name
                     (let ((len (string-length name)))
                       (substring name (- len (min 100 len)) len)))
                 100
                 0)
                (octal-field2 mode 8 100)
                (octal-field2 uid 8 108)
                (octal-field2 gid 8 116)
                (octal-field1-bignum size 12 124)
                (octal-field1-bignum mtime 12 136)
                (case type
                  ((regular)
                   (byte-field (if (eq? tar-format 'v7) 0 48) 156))
                  ((link)
                   (byte-field 49 156))
                  ((symbolic-link)
                   (byte-field 50 156))
                  ((character-special)
                   (byte-field 51 156))
                  ((block-special)
                   (byte-field 52 156))
                  ((directory)
                   (byte-field 53 156))
                  ((fifo)
                   (byte-field 54 156))
                  (else
                   (tar-illegal-field)))
                (string-field linkname 100 157)
                (case tar-format
                  ((gnu)
                   (string-field "ustar  " 8 257))
                  ((ustar)
                   (or (string-field "ustar" 6 257)
                       (string-field "00" 2 263)))
                  (else
                   #f))
                (string-field uname 32 265)
                (string-field gname 32 297)
                (and devmajor (octal-field1 devmajor 8 329))
                (and devminor (octal-field1 devminor 8 337))
                (if (eq? tar-format 'gnu)
                    (or (and atime (octal-field1-bignum atime 12 345))
                        (and ctime (octal-field1-bignum ctime 12 357)))
                    (let ((len (string-length name)))
                      (string-field
                       (substring name 0 (- len (min 100 len)))
                       155
                       345)))
                (octal-field3 (checksum) 7 148)
                (begin
                  (genport-write-subu8vector header 0 (header-size) genport-out)
                  #f)))))

      (define (write-content tr)
        (let* ((content (tar-rec-content tr))
               (size (bytevector-length content)))
          (genport-write-subu8vector content 0 size genport-out)
          (write-pad (modulo (- size) 512))))

      (define (write-tar-rec tr)
        (or (write-header tr)
            (write-content tr)))

      (define (write-tar-rec-list tar-rec-list)
        (let loop ((lst tar-rec-list))
          (if (pair? lst)
              (let ((tr (car lst)))
                (or (write-tar-rec tr)
                    (loop (cdr lst))))
              #f)))

      (let ((exc (write-tar-rec-list tar-rec-list)))
        (if exc
            (snow-raise exc)
            (write-pad (* 2 512)))))

    (define (tar-pack-file tar-rec-list filename)
      (let ((genport-out (genport-open-output-file filename)))
        (tar-pack-genport tar-rec-list genport-out)
        (genport-close-output-port genport-out)
        #f))

    (define (tar-pack-u8vector tar-rec-list)
      (let ((genport-out (genport-open-output-u8vector)))
        (tar-pack-genport tar-rec-list genport-out)
        (genport-get-output-u8vector genport-out)))

;;;----------------------------------------------------------------------------

    ;; Unpacking tar files.

    (define (tar-unpack-genport genport-in . maybe-consumer+finisher)

      ;; Error handling.

      (define (tar-file-truncated-error)
        (make-tar-condition "tar file truncated"))

      (define (tar-header-format-unrecognized-error)
        (make-tar-condition
         (string-append "tar header format unrecognized")))

      (define (tar-header-checksum-error)
        (make-tar-condition "tar header checksum error"))

      (let ((header (make-bytevector (header-size))))

        (define (string-field max-length offset)
          (string-field-aux
           (let loop ((len 0))
             (if (or (= len max-length)
                     (= (bytevector-u8-ref header (+ offset len)) 0))
                 len
                 (loop (+ len 1))))
           offset))

        (define (string-field-aux len offset)
          (let ((str (make-string len)))
            (let loop ((i 0))
              (if (< i len)
                  (begin
                    (string-set!
                     str
                     i
                     (integer->char (bytevector-u8-ref header (+ offset i))))
                    (loop (+ i 1)))
                  str))))

        (define (byte-field offset)
          (bytevector-u8-ref header offset))

        (define (octal-field-extract len offset)
          (let loop1 ((start 0))
            (if (and (< start len)
                     (= 32 (bytevector-u8-ref header (+ offset start))))
                (loop1 (+ start 1))
                (let loop2 ((end start))
                  (if (and (< end len)
                           (let ((x (bytevector-u8-ref header (+ offset end))))
                             (and (>= x 48) (<= x 55))))
                      (loop2 (+ end 1))
                      (latin-1->string
                       (bytevector-copy-partial header
                                                (+ offset start)
                                                (+ offset end))))))))

        (define (octal-field-bignum len offset)
          (cond ((= (bitwise-and (bytevector-u8-ref header offset) #x80) 0)
                 (string->bignum (octal-field-extract len offset) 8))
                ;; XXX if first byte is 255 it's negative
                (else
                 ;; binary field
                 (u8vector->bignum
                  (bytevector-copy header (+ offset 1) (+ offset len))))))

        (define (octal-field len offset)
          (string->number (octal-field-extract len offset) 8))

        (define (checksum)
          (let loop ((i 0))
            (if (< i 8)
                (begin
                  (bytevector-u8-set! header (+ 148 i) 32)
                  (loop (+ i 1)))))
          (let loop ((sum 0) (i 0))
            (if (< i (header-size))
                (loop (+ sum (bytevector-u8-ref header i))
                      (+ i 1))
                sum)))

        (define (read-header)
          (let ((n (genport-read-subu8vector
                    header 0 (header-size) genport-in)))
            (cond ((not (= n (header-size)))
                   (tar-file-truncated-error))
                  ((= (bytevector-u8-ref header 0) 0)
                   (make-tar-rec
                    #f #f #f #f #f #f #f #f #f
                    #f #f #f #f #f))
                  (else
                   (let* ((name     (string-field 100 0))
                          (mode     (octal-field 8 100))
                          (uid      (octal-field 8 108))
                          (gid      (octal-field 8 116))
                          (size     (octal-field-bignum 12 124))
                          (mtime    (octal-field-bignum 12 136))
                          (chksum   (octal-field 8 148))
                          (typeflag (byte-field 156))
                          (linkname (string-field 100 157))
                          (magicver (string-field 8 257))
                          (magic    (string-field 6 257))
                          (version  (string-field 2 263)))
                     (let* ((tar-format
                             (cond ((string=? magicver "ustar  ")
                                    'gnu)
                                   ((and (string=? magic "ustar")
                                         (string=? version "00"))
                                    'ustar)
                                   ((and (string=? magic "")
                                         (string=? version ""))
                                    'v7)
                                   (else
                                    'unknown)))
                            (gnu?
                             (eq? tar-format 'gnu)))
                       (cond ((eq? tar-format 'unknown)
                              (tar-header-format-unrecognized-error))
                             ((not (= chksum (checksum)))
                              (tar-header-checksum-error))
                             (else
                              (let ((uname (string-field 32 265))
                                    (gname (string-field 32 297))
                                    (devmajor (octal-field 8 329))
                                    (devminor (octal-field 8 337))
                                    (prefix
                                     (if gnu? "" (string-field 155 345)))
                                    (atime
                                     (if gnu? (octal-field-bignum 12 345) #f))
                                    (ctime
                                     (if gnu? (octal-field-bignum 12 357) #f))
                                    (type
                                     (case typeflag
                                       ((0 48)
                                        'regular)
                                       ((49)
                                        'link)
                                       ((50)
                                        'symbolic-link)
                                       ((51)
                                        'character-special)
                                       ((52)
                                        'block-special)
                                       ((53)
                                        'directory)
                                       ((54)
                                        'fifo)
                                       (else
                                        #f))))
                                (make-tar-rec
                                 (string-append prefix name)
                                 mode
                                 uid
                                 gid
                                 mtime
                                 type
                                 linkname
                                 uname
                                 gname
                                 devmajor
                                 devminor
                                 atime
                                 ctime
                                 size))))))))))

        (define (read-tar-file consumer)
          (let loop ()
            (let ((tar-rec (read-header)))
              (if (tar-rec? tar-rec)
                  (let ((name (tar-rec-name tar-rec)))
                    (if (not name)
                        #t
                        (let* ((size-bignum (tar-rec-content tar-rec))
                               (size (bignum->fixnum size-bignum))
                               (d-in (genport->delimted-genport
                                      genport-in size))
                               (n (consumer tar-rec size d-in)))
                          (genport-discard-until-eof d-in)
                          (if (let ((pad (modulo (- size) 512)))
                                (not (= pad
                                        (genport-read-subu8vector
                                         (make-bytevector pad)
                                         0
                                         pad
                                         genport-in))))
                              (snow-raise (tar-file-truncated-error)))
                          (loop))))
                  (snow-raise tar-rec)))))


        (define rev-tar-rec-list '())

        ;; consumer to use if the caller didn't provide one.  it
        ;; sets the content of each tar-rec and conses it to a list.
        (define (list-consumer tar-rec size genport-in)
          (let ((v (genport-read-u8vector genport-in)))
            (tar-rec-content-set! tar-rec v)
            (set! rev-tar-rec-list (cons tar-rec rev-tar-rec-list))
            (bytevector-length v)))

        ;; finisher to use if caller didn't provide one.  return's
        ;; a list of tar-recs
        (define (list-finisher)
          (reverse rev-tar-rec-list))

        (let* ((consumer (if (pair? maybe-consumer+finisher)
                             (car maybe-consumer+finisher)
                             #f))
               (finisher (if (and (pair? maybe-consumer+finisher)
                                  (pair? (cdr maybe-consumer+finisher)))
                             (cadr maybe-consumer+finisher)
                             #f)))
          (read-tar-file (if consumer consumer list-consumer))
          (cond ((not consumer) (list-finisher))
                (finisher (finisher))
                (else #t)))))

    (define (tar-unpack-file filename)
      (let* ((genport-in (genport-open-input-file filename))
             (result (tar-unpack-genport genport-in)))
        (genport-close-input-port genport-in)
        result))

    (define (tar-unpack-u8vector u8vect)
      (let* ((genport-in (genport-open-input-u8vector u8vect))
             (result (tar-unpack-genport genport-in)))
        (genport-close-input-port genport-in)
        result))

    (define (tar-read-file filename)

      (define mtime ;; current time because we can't get file's mtime
        ;; (exact (floor (current-second)))
        (snow-file-mtime filename)
        )

      (define (read-file filename rev-tar-rec-list)
        (let* ((type
                (if (snow-file-directory? filename)
                    'directory
                    'regular))
               (content
                (if (eq? type 'regular)
                    (genport-read-file filename)
                    (make-bytevector 0)))
               (mode
                   (if (eq? type 'directory) 493 420))
               (tr
                (make-tar-rec
                 (if (eq? type 'directory)
                     (snow-make-filename filename "")
                     filename)
                 mode ;; mode
                 0 ;; uid
                 0 ;; gid
                 mtime ;; mtime
                 type
                 "" ;; linkname
                 "root" ;; uname
                 "root" ;; gname
                 0 ;; devmajor
                 0 ;; devminor
                 #f ;; atime
                 #f ;; ctime
                 content))
               (new-rev-tar-rec-list
                (cons tr rev-tar-rec-list)))
          (if (eq? type 'directory)
              (read-dir filename new-rev-tar-rec-list)
              new-rev-tar-rec-list)))

      (define (read-dir dir rev-tar-rec-list)
        (let loop ((files (snow-directory-files dir))
                   (rev-tar-rec-list rev-tar-rec-list))
          (if (pair? files)
              (let* ((name
                      (car files))
                     (filename
                      (snow-make-filename dir name)))
                (loop (cdr files)
                      (read-file filename rev-tar-rec-list)))
              rev-tar-rec-list)))

      (reverse (read-file filename '())))

;;;============================================================================

    ))
