;;;=============================================================================

;;; File: "random.scm", Time-stamp: <2007-04-05 00:52:42 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;=============================================================================

;;; Provides procedures to get random bits of high quality.


(define-library (snow random)
  (export make-random-u8vector
          random-bignum
          random-fixnum)
  (import (scheme base)
          (scheme write) ;; XXX
          (srfi 27)
          (snow bytevector)
          (snow binio)
          (snow bignum))
  (begin

    (cond-expand

     ((or chibi chicken gauche sagittarius)

      ;; on chibi, calling make-random-source resets the generator
      ;; and it will produce the sequence again
      (define rs (make-random-source))
      (define mi (random-source-make-integers rs))
      (random-source-randomize! rs)

      (define (make-random-u8vector len)
        (let* (;; (rs (make-random-source))
               ;; (mi (random-source-make-integers rs))
               (u8vect (make-bytevector len)))
          ;; (random-source-randomize! rs)
          (let loop ((i 0))
            (if (< i len)
                (begin
                  (bytevector-u8-set! u8vect i (mi 256))
                  (loop (+ i 1)))))
          u8vect)))

     (else
      ;; work around a bug in Gauche
      ;; work around a bug in STklos

      (define (random-bits-file) "/dev/random")

      (define (make-random-u8vector len)
        (let* ((in (binio-open-input-file (random-bits-file)))
               (u8vect (make-bytevector len))
               (n (binio-read-subu8vector u8vect 0 len in)))
          (binio-close-input-port in)
          (if (= n len)
              u8vect
              (error "random bits file ended prematurely"))))))

;;;----------------------------------------------------------------------------

    (define (random-bignum range)
      (let* ((range-bits (bignum-integer-length range))
             (len (quotient (+ range-bits 20) 8))
             (n (bignum-expt (fixnum->bignum 256) (fixnum->bignum len)))
             (divisor (bignum-quotient n range))
             (limit (bignum* divisor range)))
        (let loop ()
          (let* ((u8vect (make-random-u8vector len))
                 (x (fixnum-list->bignum (bytevector->u8-list u8vect) 255)))
            (if (bignum>= x limit)
                (loop)
                (bignum-quotient x divisor))))))

    (define (random-fixnum range)
      (bignum->fixnum (random-bignum (fixnum->bignum range))))

;;;=============================================================================

    ))
