;;;============================================================================

;;; File: "binio.scm", Time-stamp: <2007-09-03 13:21:13 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to do binary I/O, including a subset of SRFI 91.


(define-library (snow binio)
  (export binio-input-port?
          binio-output-port?
          binio-open-input-file
          binio-open-output-file
          binio-close-input-port
          binio-close-output-port
          binio-read-byte
          binio-write-byte
          binio-read-subu8vector
          binio-write-subu8vector
          binio-write-latin-1-string
          binio-read-latin-1-line
          )
  (import (scheme base) (scheme file))
  (cond-expand
   (chibi)
   (chicken)
   (gauche (import (binary io)))
   (sagittarius))
  (import (snow bytevector))
  (begin

    (cond-expand

     (srfi-91

      (define binio-read-byte read-byte)
      (define binio-write-byte write-byte))

     (gambit

      (define binio-read-byte read-u8)
      (define binio-write-byte write-u8))

     (gauche

      (define binio-read-byte read-binary-uint8)
      (define binio-write-byte write-binary-uint8))

     (mzscheme

      (define binio-read-byte read-byte)
      (define binio-write-byte write-byte))

     (else

      (define (binio-read-byte . args)
        (let* ((args-len (length args))
               (port (if (> args-len 0)
                         (list-ref args 0)
                         (current-input-port))))
          (read-u8 port)))

      (define (binio-write-byte n . args)
        (let* ((args-len (length args))
               (port (if (> args-len 0)
                         (list-ref args 0)
                         (current-output-port))))
          (write-u8 n port)))))

    (cond-expand

     ((or srfi-91
          gambit)

      (define binio-read-subu8vector read-subu8vector)
      (define binio-write-subu8vector write-subu8vector))

     (else

      (define (binio-read-subu8vector u8vect start end . args)
        (let* ((args-len (length args))
               (port (if (> args-len 0)
                         (list-ref args 0)
                         (current-input-port))))
          (let loop ((i start))
            (if (< i end)
                (let ((n (binio-read-byte port)))
                  (if (eof-object? n)
                      (- i start)
                      (begin
                        (bytevector-u8-set! u8vect i n)
                        (loop (+ i 1)))))
                (- i start)))))

      (define (binio-write-subu8vector u8vect start end . args)
        (let* ((args-len (length args))
               (port (if (> args-len 0)
                         (list-ref args 0)
                         (current-output-port))))
          (let loop ((i start))
            (if (< i end)
                (begin
                  (binio-write-byte (bytevector-u8-ref u8vect i) port)
                  (loop (+ i 1)))
                (- i start)))))))

    (cond-expand

     (bigloo

      (define (binio-open-input-file filename)
        (let ((p (open-input-file filename)))
          (if (input-port? p)
              p
              (raise (instantiate::&io-port-error
                      (proc 'binio-open-input-file)
                      (msg "Cannot open file for input")
                      (obj filename))))))

      (define (binio-open-output-file filename)
        (let ((p (open-output-file filename)))
          (if (output-port? p)
              p
              (raise (instantiate::&io-port-error
                      (proc 'binio-open-input-file)
                      (msg "Cannot open file for output")
                      (obj filename)))))))

     (kawa

      (define (binio-open-input-file filename)
        (fluid-let ((port-char-encoding #f))
          (open-input-file filename)))

      (define (binio-open-output-file filename)
        (fluid-let ((port-char-encoding #f))
          (open-output-file filename))))

     (sisc

      (define (binio-open-input-file filename)
        (open-input-file filename "8859_1"))

      (define (binio-open-output-file filename)
        (open-output-file filename "8859_1")))

     (sagittarius

      (define (binio-open-input-file filename)
        (open-binary-input-file filename))

      (define (binio-open-output-file filename)
        (if (file-exists? filename) (delete-file filename))
        (open-binary-output-file filename)))

     (else

      (define (binio-open-input-file filename)
        (open-binary-input-file filename))

      (define (binio-open-output-file filename)
        (open-binary-output-file filename))))

;;;----------------------------------------------------------------------------

    (define (binio-input-port? port)
      (input-port? port))

    (define (binio-output-port? port)
      (output-port? port))

    (define (binio-close-input-port port)
      (close-input-port port))

    (define (binio-close-output-port port)
      (close-output-port port))

;;;============================================================================

    (define (binio-write-latin-1-string str bin-port)
      (let ((bv (string->latin-1 str)))
        (binio-write-subu8vector
         bv 0 (bytevector-length bv) bin-port)))


    (define (binio-read-latin-1-line bin-port)
      (let loop ((ret (list)))
        (let ((c (binio-read-byte bin-port)))
          (cond ((or (eof-object? c) (= c 10))
                 ;; drop any \r's off the end
                 (let loop ((ret ret))
                   (cond ((or (null? ret)
                              (not (= (car ret) 13)))
                          (list->string (map integer->char (reverse ret))))
                         (else (loop (cdr ret))))))
                (else
                 (loop (cons c ret)))))))


    ))
