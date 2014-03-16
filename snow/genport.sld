;;;============================================================================

;;; File: "genport.scm", Time-stamp: <2007-04-05 00:51:44 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides a procedural representation of I/O ports.


(define-library (snow genport)
  (export make-genport
          genport?
          genport-end
          genport-read
          genport-write
          genport-input-port?
          genport-open-input-file
          genport-native-input-port->genport
          genport-open-input-subu8vector
          genport-open-input-u8vector
          genport-close-input-port
          genport-read-subu8vector
          genport-read-u8vector
          genport-read-file
          genport-output-port?
          genport-open-output-file
          genport-native-output-port->genport
          genport-open-output-u8vector
          genport-get-output-u8vector
          genport-close-output-port
          genport-write-subu8vector
          genport-write-u8vector
          genport-write-file)
  (import (scheme base))
  (import (scheme file))
  (import (snow bytevector))
  (import (snow binio))
  (cond-expand
   (chibi)
   (chicken)
   (gauche)
   (sagittarius))
  (begin

    ;; http://wiki.call-cc.org/man/4/Unit%20ports

    ;; http://practical-scheme.net/gauche/man/gauche-refe_107.html#Virtual-ports

    ;; chibi has make-custom-input-port and make-custom-output-port and
    ;; make-generated-input-port in (chibi io)

    ;; sagittarius has make-custom-input-port ?  only shows up in a test

    (define-record-type <genport>
      (make-genport end read write)
      genport?
      (end genport-end set-genport-end!)
      (read genport-read set-genport-read!)
      (write genport-write set-genport-write!))

    (define (genport-input-port? genport)
      (and (genport? genport)
           (procedure? (genport-read genport))))

    (define (genport-open-input-file filename)
      (let ((port (binio-open-input-file filename)))
        (make-genport
         (lambda (genport)
           (close-input-port port))
         (lambda (u8vect start end genport)
           (binio-read-subu8vector u8vect start end port))
         #f)))

    (define (genport-native-input-port->genport port)
      (make-genport
       (lambda (genport)
         #f)
       (lambda (u8vect start end genport)
         (binio-read-subu8vector u8vect start end port))
       #f))

    (define (genport-open-input-subu8vector src-u8vect src-start src-end)
      (let ((pos src-start))
        (make-genport
         (lambda (genport)
           #f)
         (lambda (u8vect start end genport)
           (let ((n (min (- src-end pos) (- end start))))
             (bytevector-copy-partial! src-u8vect pos (+ pos n) u8vect start)
             (set! pos (+ pos n))
             n))
         #f)))

    (define (genport-open-input-u8vector src-u8vect)
      (genport-open-input-subu8vector
       src-u8vect
       0
       (bytevector-length src-u8vect)))

    (define (genport-close-input-port genport)
      ((genport-end genport) genport))

    (define (genport-read-subu8vector u8vect start end genport)
      ((genport-read genport) u8vect start end genport))

    (define (genport-read-u8vector genport-in)
      (define block-size 65536)
      (define (read-blocks len)
        (let* ((buf (make-bytevector block-size))
               (n (genport-read-subu8vector buf 0 block-size genport-in))
               (u8vect (if (< n block-size)
                           (make-bytevector (+ len n))
                           (read-blocks (+ len n)))))
          (bytevector-copy-partial! buf 0 n u8vect len)
          u8vect))
      (read-blocks 0))

    (define (genport-read-file filename)
      (let* ((genport-in (genport-open-input-file filename))
             (u8vect (genport-read-u8vector genport-in)))
        (genport-close-input-port genport-in)
        u8vect))

    (define (genport-output-port? genport)
      (and (genport? genport)
           (procedure? (genport-write genport))))

    (define (genport-open-output-file filename)
      (if (file-exists? filename)
          (delete-file filename))
      (let ((port (binio-open-output-file filename)))
        (make-genport
         (lambda (genport)
           (close-output-port port))
         #f
         (lambda (u8vect start end genport)
           (binio-write-subu8vector u8vect start end port)))))

    (define (genport-native-output-port->genport port)
      (make-genport
       (lambda (genport)
         #f)
       #f
       (lambda (u8vect start end genport)
         (binio-write-subu8vector u8vect start end port))))

    (define (genport-open-output-u8vector)
      (let* ((bufsize 1024)
             (buf (make-bytevector bufsize 0))
             (pos 0)
             (rev-bufs '()))
        (make-genport
         (lambda (genport)
           (if (not genport)
               (let ((result
                      (apply bytevector-append
                             (reverse (cons (bytevector-copy-partial buf 0 pos)
                                            rev-bufs)))))
                 (set! pos 0)
                 (set! rev-bufs '())
                 result)
               #f))
         #f
         (lambda (u8vect start end genport)
           (let loop ((i start) (len (- end start)))
             (if (> len 0)
                 (if (= pos bufsize)
                     (begin
                       (set! rev-bufs (cons buf rev-bufs))
                       (set! buf (make-bytevector bufsize 0))
                       (set! pos 0)
                       (loop i len))
                     (let* ((n (min len (- bufsize pos)))
                            (new-i (+ i n))
                            (new-pos (+ pos n)))
                       (bytevector-copy-partial! u8vect i new-i buf pos)
                       (set! pos new-pos)
                       (loop new-i (- len n))))
                 (- end start)))))))

    (define (genport-get-output-u8vector genport)
      ((genport-end genport) #f))

    (define (genport-close-output-port genport)
      ((genport-end genport) genport))

    (define (genport-write-subu8vector u8vect start end genport)
      ((genport-write genport) u8vect start end genport))

    (define (genport-write-u8vector u8vect genport)
      (genport-write-subu8vector u8vect 0 (bytevector-length u8vect) genport))

    (define (genport-write-file u8vect filename)
      (let ((genport-out (genport-open-output-file filename)))
        (genport-write-u8vector u8vect genport-out)
        (genport-close-output-port genport-out)))

;;;============================================================================


    ))
