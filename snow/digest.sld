;;;============================================================================

;;; File: "digest.scm", Time-stamp: <2007-04-05 00:51:05 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to compute message digests.



(define-library (snow digest)
  (export open-digest
          close-digest
          digest-update-subu8vector
          digest-update-u8
          digest-update-u16-le
          digest-update-u16-be
          digest-update-u32-le
          digest-update-u32-be
          digest-string
          digest-substring
          digest-u8vector
          digest-subu8vector
          digest-file)
  (import (scheme base)
          (scheme cxr)
          (scheme char))
  (import (srfi 60))
  (cond-expand
   (chibi (import (scheme cxr)))
   (else))
  (import (snow binio)
          (snow bytevector))
  (begin



;; #! /bin/sh
;; #| -*- scheme -*-
;; exec csi -s $0 "$@"
;; |#

;; (require-library low-level-macros)
;; (import low-level-macros macro-helpers)
;; (import-for-syntax
;;   (only low-level-macros macro-rules once-only with-gensyms))

;; (use srfi-4)
;; (define bytevector u8vector)
;; (define make-bytevector make-u8vector)
;; (define bytevector-u8-ref u8vector-ref)
;; (define bytevector-u8-set! u8vector-set!)
;; (define bytevector-length u8vector-length)

;;;============================================================================

;;; File: "digest.scm", Time-stamp: <2007-04-05 00:51:05 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to compute message digests.


;; TODO: change the interface so that a "digest" is really a generic
;; port.  This way the I/O procedures can be used to feed the content
;; of the message to digest (for example genport-write-subu8vector
;; will subsume digest-update-subu8vector).

;;;============================================================================

(define-record-type <digest>
  (make-digest end update state)
  digest?
  (end digest-end set-digest-end!)
  (update digest-update set-digest-update!)
  (state digest-state set-digest-state!))

(define (hex8 n) ;; assumes n is a fixnum
  (substring (number->string (+ n #x100) 16) 1 3))

(define (hex16 n) ;; assumes n is a fixnum
  (substring (number->string (+ n #x10000) 16) 1 5))

;;;----------------------------------------------------------------------------

;; CRC32 digest.

(define-record-type <crc32-digest>
  (make-crc32-digest hi16 lo16)
  crc32-digest?
  (hi16 crc32-digest-hi16 crc32-digest-hi16-set!)
  (lo16 crc32-digest-lo16 crc32-digest-lo16-set!))


(define crc32-table '#(
#x0000 #x0000 #x7707 #x3096 #xEE0E #x612C #x9909 #x51BA
#x076D #xC419 #x706A #xF48F #xE963 #xA535 #x9E64 #x95A3
#x0EDB #x8832 #x79DC #xB8A4 #xE0D5 #xE91E #x97D2 #xD988
#x09B6 #x4C2B #x7EB1 #x7CBD #xE7B8 #x2D07 #x90BF #x1D91
#x1DB7 #x1064 #x6AB0 #x20F2 #xF3B9 #x7148 #x84BE #x41DE
#x1ADA #xD47D #x6DDD #xE4EB #xF4D4 #xB551 #x83D3 #x85C7
#x136C #x9856 #x646B #xA8C0 #xFD62 #xF97A #x8A65 #xC9EC
#x1401 #x5C4F #x6306 #x6CD9 #xFA0F #x3D63 #x8D08 #x0DF5
#x3B6E #x20C8 #x4C69 #x105E #xD560 #x41E4 #xA267 #x7172
#x3C03 #xE4D1 #x4B04 #xD447 #xD20D #x85FD #xA50A #xB56B
#x35B5 #xA8FA #x42B2 #x986C #xDBBB #xC9D6 #xACBC #xF940
#x32D8 #x6CE3 #x45DF #x5C75 #xDCD6 #x0DCF #xABD1 #x3D59
#x26D9 #x30AC #x51DE #x003A #xC8D7 #x5180 #xBFD0 #x6116
#x21B4 #xF4B5 #x56B3 #xC423 #xCFBA #x9599 #xB8BD #xA50F
#x2802 #xB89E #x5F05 #x8808 #xC60C #xD9B2 #xB10B #xE924
#x2F6F #x7C87 #x5868 #x4C11 #xC161 #x1DAB #xB666 #x2D3D
#x76DC #x4190 #x01DB #x7106 #x98D2 #x20BC #xEFD5 #x102A
#x71B1 #x8589 #x06B6 #xB51F #x9FBF #xE4A5 #xE8B8 #xD433
#x7807 #xC9A2 #x0F00 #xF934 #x9609 #xA88E #xE10E #x9818
#x7F6A #x0DBB #x086D #x3D2D #x9164 #x6C97 #xE663 #x5C01
#x6B6B #x51F4 #x1C6C #x6162 #x8565 #x30D8 #xF262 #x004E
#x6C06 #x95ED #x1B01 #xA57B #x8208 #xF4C1 #xF50F #xC457
#x65B0 #xD9C6 #x12B7 #xE950 #x8BBE #xB8EA #xFCB9 #x887C
#x62DD #x1DDF #x15DA #x2D49 #x8CD3 #x7CF3 #xFBD4 #x4C65
#x4DB2 #x6158 #x3AB5 #x51CE #xA3BC #x0074 #xD4BB #x30E2
#x4ADF #xA541 #x3DD8 #x95D7 #xA4D1 #xC46D #xD3D6 #xF4FB
#x4369 #xE96A #x346E #xD9FC #xAD67 #x8846 #xDA60 #xB8D0
#x4404 #x2D73 #x3303 #x1DE5 #xAA0A #x4C5F #xDD0D #x7CC9
#x5005 #x713C #x2702 #x41AA #xBE0B #x1010 #xC90C #x2086
#x5768 #xB525 #x206F #x85B3 #xB966 #xD409 #xCE61 #xE49F
#x5EDE #xF90E #x29D9 #xC998 #xB0D0 #x9822 #xC7D7 #xA8B4
#x59B3 #x3D17 #x2EB4 #x0D81 #xB7BD #x5C3B #xC0BA #x6CAD
#xEDB8 #x8320 #x9ABF #xB3B6 #x03B6 #xE20C #x74B1 #xD29A
#xEAD5 #x4739 #x9DD2 #x77AF #x04DB #x2615 #x73DC #x1683
#xE363 #x0B12 #x9464 #x3B84 #x0D6D #x6A3E #x7A6A #x5AA8
#xE40E #xCF0B #x9309 #xFF9D #x0A00 #xAE27 #x7D07 #x9EB1
#xF00F #x9344 #x8708 #xA3D2 #x1E01 #xF268 #x6906 #xC2FE
#xF762 #x575D #x8065 #x67CB #x196C #x3671 #x6E6B #x06E7
#xFED4 #x1B76 #x89D3 #x2BE0 #x10DA #x7A5A #x67DD #x4ACC
#xF9B9 #xDF6F #x8EBE #xEFF9 #x17B7 #xBE43 #x60B0 #x8ED5
#xD6D6 #xA3E8 #xA1D1 #x937E #x38D8 #xC2C4 #x4FDF #xF252
#xD1BB #x67F1 #xA6BC #x5767 #x3FB5 #x06DD #x48B2 #x364B
#xD80D #x2BDA #xAF0A #x1B4C #x3603 #x4AF6 #x4104 #x7A60
#xDF60 #xEFC3 #xA867 #xDF55 #x316E #x8EEF #x4669 #xBE79
#xCB61 #xB38C #xBC66 #x831A #x256F #xD2A0 #x5268 #xE236
#xCC0C #x7795 #xBB0B #x4703 #x2202 #x16B9 #x5505 #x262F
#xC5BA #x3BBE #xB2BD #x0B28 #x2BB4 #x5A92 #x5CB3 #x6A04
#xC2D7 #xFFA7 #xB5D0 #xCF31 #x2CD9 #x9E8B #x5BDE #xAE1D
#x9B64 #xC2B0 #xEC63 #xF226 #x756A #xA39C #x026D #x930A
#x9C09 #x06A9 #xEB0E #x363F #x7207 #x6785 #x0500 #x5713
#x95BF #x4A82 #xE2B8 #x7A14 #x7BB1 #x2BAE #x0CB6 #x1B38
#x92D2 #x8E9B #xE5D5 #xBE0D #x7CDC #xEFB7 #x0BDB #xDF21
#x86D3 #xD2D4 #xF1D4 #xE242 #x68DD #xB3F8 #x1FDA #x836E
#x81BE #x16CD #xF6B9 #x265B #x6FB0 #x77E1 #x18B7 #x4777
#x8808 #x5AE6 #xFF0F #x6A70 #x6606 #x3BCA #x1101 #x0B5C
#x8F65 #x9EFF #xF862 #xAE69 #x616B #xFFD3 #x166C #xCF45
#xA00A #xE278 #xD70D #xD2EE #x4E04 #x8354 #x3903 #xB3C2
#xA767 #x2661 #xD060 #x16F7 #x4969 #x474D #x3E6E #x77DB
#xAED1 #x6A4A #xD9D6 #x5ADC #x40DF #x0B66 #x37D8 #x3BF0
#xA9BC #xAE53 #xDEBB #x9EC5 #x47B2 #xCF7F #x30B5 #xFFE9
#xBDBD #xF21C #xCABA #xC28A #x53B3 #x9330 #x24B4 #xA3A6
#xBAD0 #x3605 #xCDD7 #x0693 #x54DE #x5729 #x23D9 #x67BF
#xB366 #x7A2E #xC461 #x4AB8 #x5D68 #x1B02 #x2A6F #x2B94
#xB40B #xBE37 #xC30C #x8EA1 #x5A05 #xDF1B #x2D02 #xEF8D
))

(define (digest-update-crc32 digest u8vect start end)
  (let ((state (digest-state digest)))
    (let ((hi16 (crc32-digest-hi16 state))
          (lo16 (crc32-digest-lo16 state)))
      (let loop ((i start)
                 (hi hi16)
                 (lo lo16))
        (if (< i end)
            (let* ((x (bytevector-u8-ref u8vect i))
                   (j (* 2 (bitwise-and (bitwise-xor lo x) #xff)))
                   (crc-hi (vector-ref crc32-table j))
                   (crc-lo (vector-ref crc32-table (+ j 1))))
              (loop (+ i 1)
                    (bitwise-xor
                     crc-hi
                     (arithmetic-shift hi (- 8)))
                    (bitwise-xor
                     crc-lo
                     (+ (arithmetic-shift (bitwise-and hi #xff) 8)
                        (arithmetic-shift lo (- 8))))))
            (begin
              (crc32-digest-hi16-set! state hi)
              (crc32-digest-lo16-set! state lo)))))))

(define (end-crc32 digest result-type)
  (let ((state (digest-state digest)))
    (let ((hi16 (bitwise-xor (crc32-digest-hi16 state) #xffff))
          (lo16 (bitwise-xor (crc32-digest-lo16 state) #xffff)))
      (case result-type
        ((hex)
         (string-append (hex16 hi16) (hex16 lo16)))
        ((u8vector)
         (bytevector (bitwise-and #xff lo16)
                        (arithmetic-shift lo16 (- 8))
                        (bitwise-and #xff hi16)
                        (arithmetic-shift hi16 (- 8))))
        (else
         (error "unsupported digest result-type" result-type))))))

(define (open-digest-crc32)
  (make-digest
   end-crc32
   digest-update-crc32
   (make-crc32-digest #xffff #xffff)))

;; Useful for debugging:
;;
;;(define crc32-poly #xEDB88320)
;;
;;(define crc32-table-computed
;;  (let ((v (make-vector 512)))
;;    (let loop1 ((n 0))
;;      (if (< n 256)
;;          (let loop2 ((k 0) (c n))
;;            (if (< k 8)
;;                (loop2 (+ k 1)
;;                       (if (odd? c)
;;                           (bitwise-xor crc32-poly
;;                                        (arithmetic-shift c -1))
;;                           (arithmetic-shift c -1)))
;;                (begin
;;                  (vector-set! v (* n 2) (arithmetic-shift c -16))
;;                  (vector-set! v (+ 1 (* n 2)) (bitwise-and c #xffff))
;;                  (loop1 (+ n 1)))))))
;;    v))
;;
;;(if (not (equal? crc32-table-computed crc32-table))
;;    (error "crc32-table is incorrect"))
;;
;;(define (digest-update-crc32 digest u8vect start end)
;;  (let ((state (digest-state digest)))
;;    (let ((hi16 (crc32-digest-hi16 state))
;;          (lo16 (crc32-digest-lo16 state)))
;;      (let loop ((i start)
;;                 (crc (+ (arithmetic-shift hi16 16)
;;                         lo16)))
;;        (if (< i end)
;;            (let* ((x (bytevector-u8-ref u8vect i))
;;                   (crc (bitwise-xor crc x)))
;;              (let loop2 ((j 8) (crc crc))
;;                (if (> j 0)
;;                    (loop2 (- j 1)
;;                           (if (odd? crc)
;;                               (bitwise-xor crc32-poly
;;                                            (arithmetic-shift crc -1))
;;                               (arithmetic-shift crc -1)))
;;                    (loop (+ i 1)
;;                          crc))))
;;            (begin
;;              (crc32-digest-hi16-set!
;;               state
;;               (arithmetic-shift crc -16))
;;              (crc32-digest-lo16-set!
;;               state
;;               (bitwise-and crc #xffff))))))))

;;;----------------------------------------------------------------------------

(define (hash-block->hex-string hb big-endian? width)

  (define (hex x)
    (string-ref "0123456789abcdef" (bitwise-and x 15)))

  (let* ((len (quotient width 16))
         (n (* len 4))
         (str (make-string n)))
    (let loop ((i (- len 1)) (j (- n 4)))
      (if (< i 0)
          str
          (let ((x (vector-ref hb (if big-endian? (bitwise-xor i 1) i)))
                (j1 (if big-endian? (+ j 0) (+ j 2)))
                (j2 (if big-endian? (+ j 2) (+ j 0))))
            (string-set!
             str
             (+ j1 0)
             (hex (arithmetic-shift x (- 12))))
            (string-set!
             str
             (+ j1 1)
             (hex (arithmetic-shift x (- 8))))
            (string-set!
             str
             (+ j2 0)
             (hex (arithmetic-shift x (- 4))))
            (string-set!
             str
             (+ j2 1)
             (hex (arithmetic-shift x (- 0))))
            (loop (- i 1) (- j 4)))))))

(define (hash-block->u8vector hb big-endian? width)
  (let* ((len (quotient width 16))
         (n (* len 2))
         (u8vect (make-bytevector n 0)))
    (let loop ((i (- len 1)) (j (- n 2)))
      (if (< i 0)
          u8vect
          (let ((x (vector-ref hb (if big-endian? (bitwise-xor i 1) i)))
                (j1 (if big-endian? (+ j 0) (+ j 1)))
                (j2 (if big-endian? (+ j 1) (+ j 0))))
            (bytevector-u8-set! u8vect j1 (arithmetic-shift x (- 8)))
            (bytevector-u8-set! u8vect j2 (bitwise-and #xff x))
            (loop (- i 1) (- j 2)))))))

;; (define-macro (LO var)
;;   (string->symbol
;;    (string-append (symbol->string var) "-" (symbol->string 'L))))

;; (define-macro (HI var)
;;   (string->symbol
;;    (string-append (symbol->string var) "-" (symbol->string 'H))))

;; (define-macro (wlet var lo hi body)
;;   `(let ((,(string->symbol
;;             (string-append (symbol->string var) "-" (symbol->string 'L)))
;;           ,lo)
;;          (,(string->symbol
;;             (string-append (symbol->string var) "-" (symbol->string 'H)))
;;           ,hi))
;;      ,body))

;; (define-macro (cast-u16 x)
;;   `(bitwise-and #xffff ,x))

;; (define-macro (shift-left-u16 n shift)
;;   `(arithmetic-shift
;;     (bitwise-and ,n ,(- (expt 2 (- 16 shift)) 1))
;;     ,shift))

;; (define-macro (wshr dst w r body)
;;   (if (< r 16)
;;       `(wlet ,dst
;;              (bitwise-ior
;;               (arithmetic-shift (LO ,w) (- ,r))
;;               (shift-left-u16 (HI ,w) ,(- 16 r)))
;;              (arithmetic-shift (HI ,w) (- ,r))
;;              ,body)
;;       `(wlet ,dst
;;              (arithmetic-shift (HI ,w) (- ,(- r 16)))
;;              0
;;              ,body)))

;; (define-macro (wrot dst w r body)
;;   (if (< r 16)
;;       `(wlet ,dst
;;              (bitwise-ior
;;               (shift-left-u16 (LO ,w) ,r)
;;               (arithmetic-shift (HI ,w) (- ,(- 16 r))))
;;              (bitwise-ior
;;               (shift-left-u16 (HI ,w) ,r)
;;               (arithmetic-shift (LO ,w) (- ,(- 16 r))))
;;              ,body)
;;       `(wlet ,dst
;;              (bitwise-ior
;;               (shift-left-u16 (HI ,w) ,(- r 16))
;;               (arithmetic-shift (LO ,w) (- ,(- 32 r))))
;;              (bitwise-ior
;;               (shift-left-u16 (LO ,w) ,(- r 16))
;;               (arithmetic-shift (HI ,w) (- ,(- 32 r))))
;;              ,body)))

;; (define-macro (wadd dst a b body)
;;   `(wlet R
;;          (+ (LO ,a) (LO ,b))
;;          (+ (HI ,a) (HI ,b))
;;          (wlet ,dst
;;                (cast-u16 (LO R))
;;                (cast-u16
;;                 (+ (HI R)
;;                    (arithmetic-shift (LO R) (- 16))))
;;                ,body)))

;; (define-macro (wxor dst a b body)
;;   `(wlet ,dst
;;          (bitwise-xor (LO ,a) (LO ,b))
;;          (bitwise-xor (HI ,a) (HI ,b))
;;          ,body))

;; (define-macro (wior dst a b body)
;;   `(wlet ,dst
;;          (bitwise-ior (LO ,a) (LO ,b))
;;          (bitwise-ior (HI ,a) (HI ,b))
;;          ,body))

;; (define-macro (wand dst a b body)
;;   `(wlet ,dst
;;          (bitwise-and (LO ,a) (LO ,b))
;;          (bitwise-and (HI ,a) (HI ,b))
;;          ,body))

;; (define-macro (wnot dst a body)
;;   `(wlet ,dst
;;          (bitwise-not (LO ,a))
;;          (bitwise-not (HI ,a))
;;          ,body))

;; (define-macro (wref dst v i body)
;;   (if (number? i)
;;       `(wlet ,dst
;;              (vector-ref ,v ,(+ (* 2 i) 0))
;;              (vector-ref ,v ,(+ (* 2 i) 1))
;;              ,body)
;;       `(wlet ,dst
;;              (vector-ref ,v (+ (* 2 ,i) 0))
;;              (vector-ref ,v (+ (* 2 ,i) 1))
;;              ,body)))

;; (define-macro (wset v i x)
;;   (if (number? i)
;;       `(begin
;;          (vector-set! ,v ,(+ (* 2 i) 0) (LO ,x))
;;          (vector-set! ,v ,(+ (* 2 i) 1) (HI ,x)))
;;       `(begin
;;          (vector-set! ,v (+ (* 2 ,i) 0) (LO ,x))
;;          (vector-set! ,v (+ (* 2 ,i) 1) (HI ,x)))))




;; (define (LO var)
;;   (let-values (((var-lo var-hi) var))
;;     var-lo))

(define (LO var) (car var))

;; (define (HI var)
;;   (let-values (((var-lo var-hi) var))
;;     var-hi))

(define (HI var) (cdr var))


;; (define-syntax wlet
;;   (syntax-rules ()
;;     ((_ var lo hi body ...)
;;      (let ((var (values lo hi)))
;;        body ...))))

(define-syntax wlet
  (syntax-rules ()
    ((_ var lo hi body ...)
     (let ((var (cons lo hi)))
       body ...))))


(define (cast-u16 x)
  (bitwise-and #xffff x))

(define (shift-left-u16 n shift)
  (arithmetic-shift
   (bitwise-and n (- (expt 2 (- 16 shift)) 1))
   shift))


(define-syntax wshr
  (syntax-rules ()
    ((_ dst w r body ...)
     (wlet dst
           (if (< r 16) 
               (bitwise-ior
                (arithmetic-shift (LO w) (- r))
                (shift-left-u16 (HI w) (- 16 r)))
               (arithmetic-shift (HI w) (- (- r 16))))
           (if (< r 16)
               (arithmetic-shift (HI w) (- r))
               0)
           body ...))))

(define-syntax wrot
  (syntax-rules ()
    ((_  dst w r body ...)
     (wlet dst
           (if (< r 16)
               (bitwise-ior
                (shift-left-u16 (LO w) r)
                (arithmetic-shift (HI w) (- (- 16 r))))
               (bitwise-ior
                (shift-left-u16 (HI w) (- r 16))
                (arithmetic-shift (LO w) (- (- 32 r)))))
           (if (< r 16)
               (bitwise-ior
                (shift-left-u16 (HI w) r)
                (arithmetic-shift (LO w) (- (- 16 r))))
               (bitwise-ior
                (shift-left-u16 (LO w) (- r 16))
                (arithmetic-shift (HI w) (- (- 32 r)))))
           body ...))))


(define-syntax wadd
  (syntax-rules ()
    ((_  dst a b body ...)
     (wlet R
           (+ (LO a) (LO b))
           (+ (HI a) (HI b))
           (wlet dst
                 (cast-u16 (LO R))
                 (cast-u16
                  (+ (HI R)
                     (arithmetic-shift (LO R) (- 16))))
                 body ...)))))


(define-syntax wxor
  (syntax-rules ()
    ((_ dst a b body ...)
     (wlet dst
           (bitwise-xor (LO a) (LO b))
           (bitwise-xor (HI a) (HI b))
           body ...))))


(define-syntax wior
  (syntax-rules ()
    ((_  dst a b body ...)
     (wlet dst
           (bitwise-ior (LO a) (LO b))
           (bitwise-ior (HI a) (HI b))
           body ...))))


(define-syntax wand
  (syntax-rules ()
    ((_  dst a b body ...)
     (wlet dst
           (bitwise-and (LO a) (LO b))
           (bitwise-and (HI a) (HI b))
           body ...))))


(define-syntax wnot
  (syntax-rules ()
    ((_  dst a body ...)
     (wlet dst
           (bitwise-and (bitwise-not (LO a)) #xffff)
           (bitwise-and (bitwise-not (HI a)) #xffff)
           body ...))))


(define-syntax wref
  (syntax-rules ()
    ((_  dst v i body ...)
     (wlet dst
           (vector-ref v (+ (* 2 i) 0))
           (vector-ref v (+ (* 2 i) 1))
           body ...))))


(define (wset v i x)
  (vector-set! v (+ (* 2 i) 0) (LO x))
  (vector-set! v (+ (* 2 i) 1) (HI x)))



(define (wbitwise-ior a b)
  (cons
   (bitwise-ior (LO a) (LO b))
   (bitwise-ior (HI a) (HI b))))


(define (wbitwise-and a b)
  (cons
   (bitwise-and (LO a) (LO b))
   (bitwise-and (HI a) (HI b))))


(define (wbitwise-xor a b)
  (cons
   (bitwise-xor (LO a) (LO b))
   (bitwise-xor (HI a) (HI b))))


(define (wbitwise-not a)
  (cons
   (bitwise-and (bitwise-not (LO a)) #xffff)
   (bitwise-and (bitwise-not (HI a)) #xffff)))


;;;----------------------------------------------------------------------------

(define-record-type <block-digest>
  (make-block-digest hash-update hash block block-pos
                     bit-pos big-endian? width)
  block-digest?
  (hash-update block-digest-hash-update set-block-digest-hash-update!)
  (hash block-digest-hash set-block-digest-hash!)
  (block block-digest-block set-block-digest-block!)
  (block-pos block-digest-block-pos block-digest-block-pos-set!)
  (bit-pos block-digest-bit-pos block-digest-bit-pos-set!)
  (big-endian? block-digest-big-endian? set-block-digest-big-endian?!)
  (width block-digest-width set-block-digest-width!))


(define (convert-hash-block digest result-type)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd)))
    (case result-type
      ((hex)
       (hash-block->hex-string
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      ((u8vector)
       (hash-block->u8vector
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      (else
       (error "unsupported digest result-type" result-type)))))

(define (process-last-block digest)
  (let* ((bd
          (digest-state digest))
         (block-pos
          (block-digest-block-pos bd))
         (bit-pos
          (block-digest-bit-pos bd))
         (buf
          (make-bytevector 8 0)))

    (digest-update-u8 digest #x80) ;; add byte-aligned 1 bit

    (let ((zero-padding-bytes
           (quotient
            (bitwise-and 511 (- 448 (block-digest-bit-pos bd)))
            8)))
      (let loop1 ((n zero-padding-bytes))
        (if (< 0 n)
            (let ((m (min 8 n)))
              (digest-update-subu8vector
               digest
               buf
               0
               m) ;; add 0 bits
              (loop1 (- n m))))))

    (bytevector-u8-set!
     buf
     0
     (bitwise-and #xff bit-pos))

    (bytevector-u8-set!
     buf
     1
     (bitwise-ior
      (arithmetic-shift (bitwise-and #x7f block-pos) 1)
      (bitwise-and #x01 (arithmetic-shift bit-pos (- 8)))))

    (let loop2 ((i 2)
                (n (arithmetic-shift block-pos (- 7))))
      (if (> n 0)
          (begin
            (bytevector-u8-set! buf i (bitwise-and #xff n))
            (loop2 (+ i 1)
                   (arithmetic-shift n (- 8))))))

    (if (block-digest-big-endian? bd)
        (let loop3 ((i 3))
          (if (>= i 0)
              (let ((t (bytevector-u8-ref buf i)))
                (bytevector-u8-set! buf i (bytevector-u8-ref buf (- 7 i)))
                (bytevector-u8-set! buf (- 7 i) t)
                (loop3 (- i 1))))))

    (digest-update-subu8vector digest buf 0 8)));; add message length (in bits)

(define (end-block-digest digest result-type)
  (process-last-block digest)
  (convert-hash-block digest result-type))

(define (digest-update-block-digest digest u8vect start end)
  (let* ((bd (digest-state digest))
         (block (block-digest-block bd)))

    (define (aligned8 i bit-pos)

      ;; bit-pos is a multiple of 8

      (if (< i end)
          (let ((j (arithmetic-shift bit-pos (- 4))))
            (if (= 0 (bitwise-and bit-pos 15))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (bitwise-xor j 1)))
                        (vector-set!
                         block
                         j
                         (arithmetic-shift
                          (bytevector-u8-ref u8vect i)
                          8)))
                      (vector-set!
                       block
                       j
                       (bytevector-u8-ref u8vect i)))
                  (let ((new-bit-pos (+ bit-pos 8)))
                    (aligned8 (+ i 1) new-bit-pos)))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (bitwise-xor j 1)))
                        (vector-set!
                         block
                         j
                         (+ (vector-ref block j)
                            (bytevector-u8-ref u8vect i))))
                      (vector-set!
                       block
                       j
                       (+ (vector-ref block j)
                          (arithmetic-shift
                           (bytevector-u8-ref u8vect i)
                           8))))
                  (let ((new-bit-pos (+ bit-pos 8)))
                    (if (= 512 new-bit-pos)
                      (begin
                        ((block-digest-hash-update bd) digest)
                        (block-digest-block-pos-set!
                         bd
                         (+ (block-digest-block-pos bd) 1))
                        (aligned16 (+ i 1) 0))
                      (aligned16 (+ i 1) new-bit-pos))))))
          (block-digest-bit-pos-set! bd bit-pos)))

    (define (aligned16 i bit-pos)

      ;; bit-pos is a multiple of 16

      (if (< (+ i 1) end)
          (let ((j (arithmetic-shift bit-pos (- 4))))
            (if (block-digest-big-endian? bd)
                (let ((j (bitwise-xor j 1)))
                  (vector-set!
                   block
                   j
                   (+
                    (arithmetic-shift
                     (bytevector-u8-ref u8vect i)
                     8)
                    (bytevector-u8-ref u8vect (+ i 1)))))
                (vector-set!
                 block
                 j
                 (+
                  (arithmetic-shift
                   (bytevector-u8-ref u8vect (+ i 1))
                   8)
                  (bytevector-u8-ref u8vect i))))
            (let ((new-bit-pos (+ bit-pos 16)))
              (if (= 512 new-bit-pos)
                  (begin
                    ((block-digest-hash-update bd) digest)
                    (block-digest-block-pos-set!
                     bd
                     (+ (block-digest-block-pos bd) 1))
                    (aligned16 (+ i 2) 0))
                  (aligned16 (+ i 2) new-bit-pos))))
          (aligned8 i bit-pos)))

    (let ((bit-pos (block-digest-bit-pos bd)))
      (cond ((= 0 (bitwise-and bit-pos 15)) ;; 16 bit boundary?
             (aligned16 start bit-pos))
            (else
             ;; (= 0 (bitwise-and bit-pos 7)) ;; 8 bit boundary?
             (aligned8 start bit-pos))))))

;;;----------------------------------------------------------------------------

;; MD5 digest.

(define (hash-block-init-md5)
  (vector #x2301 #x6745
          #xab89 #xefcd
          #xdcfe #x98ba
          #x5476 #x1032))


(define-syntax wstp
  (syntax-rules ()
    ((_ block dst w f i n-hi16 n-lo16 r body ...)
     (let ((f-value f))
     (wlet T1 (LO f-value) (HI f-value)
            ;; (cons (car f) (map (lambda (v) (LO v)) (cdr f)))
            ;; (cons (car f) (map (lambda (v) (HI v)) (cdr f)))
      (wadd T2 dst T1
      (wref T3 block i
      (wadd T4 T2 T3
      (wlet T5
            n-lo16
            n-hi16
      (wadd T6 T4 T5
      (wrot T7 T6 r
      (wadd dst w T7
            body ...))))))))))))


(define (fn-F x y z)
  (wbitwise-ior
   (wbitwise-and x y)
   (wbitwise-and (wbitwise-not x) z)))

(define (fn-G x y z)
  (wbitwise-ior
   (wbitwise-and x z)
   (wbitwise-and y (wbitwise-not z))))

(define (fn-H x y z)
  (wbitwise-xor x (wbitwise-xor y z)))

(define (fn-I x y z)
  (cons
   (cast-u16
    (LO
     (wbitwise-xor
      y
      (wbitwise-ior
       x
       (wbitwise-not z)))))
   0))



(define (digest-update-md5 digest)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd))
         (block (block-digest-block bd)))

    (define (stage1 A B C D)
      (wstp block A B (fn-F B C D)  0 #xD76A #xA478  7
      (wstp block D A (fn-F A B C)  1 #xE8C7 #xB756 12
      (wstp block C D (fn-F D A B)  2 #x2420 #x70DB 17
      (wstp block B C (fn-F C D A)  3 #xC1BD #xCEEE 22
      (wstp block A B (fn-F B C D)  4 #xF57C #x0FAF  7
      (wstp block D A (fn-F A B C)  5 #x4787 #xC62A 12
      (wstp block C D (fn-F D A B)  6 #xA830 #x4613 17
      (wstp block B C (fn-F C D A)  7 #xFD46 #x9501 22
      (stage2 A B C D))))))))))

    (define (stage2 A B C D)
      (wstp block A B (fn-F B C D)  8 #x6980 #x98D8  7
      (wstp block D A (fn-F A B C)  9 #x8B44 #xF7AF 12
      (wstp block C D (fn-F D A B) 10 #xFFFF #x5BB1 17
      (wstp block B C (fn-F C D A) 11 #x895C #xD7BE 22
      (wstp block A B (fn-F B C D) 12 #x6B90 #x1122  7
      (wstp block D A (fn-F A B C) 13 #xFD98 #x7193 12
      (wstp block C D (fn-F D A B) 14 #xA679 #x438E 17
      (wstp block B C (fn-F C D A) 15 #x49B4 #x0821 22
      (stage3 A B C D))))))))))

    (define (stage3 A B C D)
      (wstp block A B (fn-G B C D)  1 #xF61E #x2562  5
      (wstp block D A (fn-G A B C)  6 #xC040 #xB340  9
      (wstp block C D (fn-G D A B) 11 #x265E #x5A51 14
      (wstp block B C (fn-G C D A)  0 #xE9B6 #xC7AA 20
      (wstp block A B (fn-G B C D)  5 #xD62F #x105D  5
      (wstp block D A (fn-G A B C) 10 #x0244 #x1453  9
      (wstp block C D (fn-G D A B) 15 #xD8A1 #xE681 14
      (wstp block B C (fn-G C D A)  4 #xE7D3 #xFBC8 20
      (stage4 A B C D))))))))))

    (define (stage4 A B C D)
      (wstp block A B (fn-G B C D)  9 #x21E1 #xCDE6  5
      (wstp block D A (fn-G A B C) 14 #xC337 #x07D6  9
      (wstp block C D (fn-G D A B)  3 #xF4D5 #x0D87 14
      (wstp block B C (fn-G C D A)  8 #x455A #x14ED 20
      (wstp block A B (fn-G B C D) 13 #xA9E3 #xE905  5
      (wstp block D A (fn-G A B C)  2 #xFCEF #xA3F8  9
      (wstp block C D (fn-G D A B)  7 #x676F #x02D9 14
      (wstp block B C (fn-G C D A) 12 #x8D2A #x4C8A 20
      (stage5 A B C D))))))))))

    (define (stage5 A B C D)
      (wstp block A B (fn-H B C D)  5 #xFFFA #x3942  4
      (wstp block D A (fn-H A B C)  8 #x8771 #xF681 11
      (wstp block C D (fn-H D A B) 11 #x6D9D #x6122 16
      (wstp block B C (fn-H C D A) 14 #xFDE5 #x380C 23
      (wstp block A B (fn-H B C D)  1 #xA4BE #xEA44  4
      (wstp block D A (fn-H A B C)  4 #x4BDE #xCFA9 11
      (wstp block C D (fn-H D A B)  7 #xF6BB #x4B60 16
      (wstp block B C (fn-H C D A) 10 #xBEBF #xBC70 23
      (stage6 A B C D))))))))))

    (define (stage6 A B C D)
      (wstp block A B (fn-H B C D) 13 #x289B #x7EC6  4
      (wstp block D A (fn-H A B C)  0 #xEAA1 #x27FA 11
      (wstp block C D (fn-H D A B)  3 #xD4EF #x3085 16
      (wstp block B C (fn-H C D A)  6 #x0488 #x1D05 23
      (wstp block A B (fn-H B C D)  9 #xD9D4 #xD039  4
      (wstp block D A (fn-H A B C) 12 #xE6DB #x99E5 11
      (wstp block C D (fn-H D A B) 15 #x1FA2 #x7CF8 16
      (wstp block B C (fn-H C D A)  2 #xC4AC #x5665 23
      (stage7 A B C D))))))))))

    (define (stage7 A B C D)
      (wstp block A B (fn-I B C D)  0 #xF429 #x2244  6
      (wstp block D A (fn-I A B C)  7 #x432A #xFF97 10
      (wstp block C D (fn-I D A B) 14 #xAB94 #x23A7 15
      (wstp block B C (fn-I C D A)  5 #xFC93 #xA039 21
      (wstp block A B (fn-I B C D) 12 #x655B #x59C3  6
      (wstp block D A (fn-I A B C)  3 #x8F0C #xCC92 10
      (wstp block C D (fn-I D A B) 10 #xFFEF #xF47D 15
      (wstp block B C (fn-I C D A)  1 #x8584 #x5DD1 21
      (stage8 A B C D))))))))))

    (define (stage8 A B C D)
      (wstp block A B (fn-I B C D)  8 #x6FA8 #x7E4F  6
      (wstp block D A (fn-I A B C) 15 #xFE2C #xE6E0 10
      (wstp block C D (fn-I D A B)  6 #xA301 #x4314 15
      (wstp block B C (fn-I C D A) 13 #x4E08 #x11A1 21
      (wstp block A B (fn-I B C D)  4 #xF753 #x7E82  6
      (wstp block D A (fn-I A B C) 11 #xBD3A #xF235 10
      (wstp block C D (fn-I D A B)  2 #x2AD7 #xD2BB 15
      (wstp block B C (fn-I C D A)  9 #xEB86 #xD391 21
      (stage9 A B C D))))))))))

    (define (stage9 A B C D)
      (wref AA hash 0 (wadd A AA A (wset hash 0 A)))
      (wref BB hash 1 (wadd B BB B (wset hash 1 B)))
      (wref CC hash 2 (wadd C CC C (wset hash 2 C)))
      (wref DD hash 3 (wadd D DD D (wset hash 3 D))))

    (wref A hash 0
    (wref B hash 1
    (wref C hash 2
    (wref D hash 3
    (stage1 A B C D)))))))


(define (open-digest-md5)
  (make-digest
   end-block-digest
   digest-update-block-digest
   (make-block-digest
    digest-update-md5
    (hash-block-init-md5)
    (make-vector 32 0)
    0
    0
    #f
    128)))

;;;----------------------------------------------------------------------------

;; SHA-1 digest.

;; (define (hash-block-init-sha-1)
;;   (vector #x2301 #x6745
;;           #xab89 #xefcd
;;           #xdcfe #x98ba
;;           #x5476 #x1032
;;           #xe1f0 #xc3d2))

;; (define (digest-update-sha-1 digest)
;;   (let* ((bd (digest-state digest))
;;          (hash (block-digest-hash bd))
;;          (block (block-digest-block bd)))
;;     (wref OLDA hash 0
;;     (wref OLDB hash 1
;;     (wref OLDC hash 2
;;     (wref OLDD hash 3
;;     (wref OLDE hash 4
;;     (let loop ((j 0)
;;                (A-L OLDA-L) (A-H OLDA-H)
;;                (B-L OLDB-L) (B-H OLDB-H)
;;                (C-L OLDC-L) (C-H OLDC-H)
;;                (D-L OLDD-L) (D-H OLDD-H)
;;                (E-L OLDE-L) (E-H OLDE-H))

;;       (define (stage1)
;;         (if (< j 16)

;;             (wref T1 block j
;;             (stage2 T1-L T1-H))

;;             (wref T1 block (- j 3)
;;             (wref T2 block (- j 8)
;;             (wxor T3 T1 T2
;;             (wref T4 block (- j 14)
;;             (wxor T5 T3 T4
;;             (wref T6 block (- j 16)
;;             (wxor T7 T5 T6
;;             (wrot X T7 1
;;             (begin
;;               (wset block j X)
;;               (stage2 X-L X-H))))))))))))

;;       (define (stage2 X-L X-H)
;;         (cond ((< j 20)
;;                (wand T1 B C
;;                (wnot T2 B
;;                (wand T3 D T2
;;                (wior T4 T1 T3
;;                (wlet T5 #x7999 #x5a82
;;                (wadd T6 T4 T5
;;                (stage3 X-L X-H T6-L T6-H))))))))
;;               ((< j 40)
;;                (wxor T1 B C
;;                (wxor T2 D T1
;;                (wlet T3 #xeba1 #x6ed9
;;                (wadd T4 T2 T3
;;                (stage3 X-L X-H T4-L T4-H))))))
;;               ((< j 60)
;;                (wand T1 B C
;;                (wand T2 B D
;;                (wior T3 T1 T2
;;                (wand T4 C D
;;                (wior T5 T3 T4
;;                (wlet T6 #xbcdc #x8f1b
;;                (wadd T7 T5 T6
;;                (stage3 X-L X-H T7-L T7-H)))))))))
;;               (else
;;                (wxor T1 B C
;;                (wxor T2 D T1
;;                (wlet T3 #xc1d6 #xca62
;;                (wadd T4 T2 T3
;;                (stage3 X-L X-H T4-L T4-H))))))))

;;       (define (stage3 X-L X-H Y-L Y-H)
;;         (wrot T1 A 5
;;         (wadd T2 E T1
;;         (wadd T3 X T2
;;         (wadd T4 Y T3
;;         (wrot T5 B 30
;;         (loop (+ j 1)
;;               T4-L T4-H
;;               A-L A-H
;;               T5-L T5-H
;;               C-L C-H
;;               D-L D-H)))))))

;;       (if (< j 80)

;;           (stage1)

;;           (begin
;;             (wadd NEWA A OLDA (wset hash 0 NEWA))
;;             (wadd NEWB B OLDB (wset hash 1 NEWB))
;;             (wadd NEWC C OLDC (wset hash 2 NEWC))
;;             (wadd NEWD D OLDD (wset hash 3 NEWD))
;;             (wadd NEWE E OLDE (wset hash 4 NEWE))))))))))))

;; (define (open-digest-sha-1)
;;   (make-digest
;;    end-block-digest
;;    digest-update-block-digest
;;    (make-block-digest
;;     digest-update-sha-1
;;     (hash-block-init-sha-1)
;;     (make-vector 160 0)
;;     0
;;     0
;;     #t
;;     160)))

;;;----------------------------------------------------------------------------

;; SHA-224 and SHA-256 digests.

;; The code below is correct, but it is commented out because it
;; triggers some bugs in the Gauche and STklos interpreters.  Gauche
;; accesses the wrong lexical variables in procedure stage4.  STklos
;; gives a bus error probably at macro expansion time.

;;(define (hash-block-init-sha-224)
;;  (vector #x9ed8 #xc105
;;          #xd507 #x367c
;;          #xdd17 #x3070
;;          #x5939 #xf70e
;;          #x0b31 #xffc0
;;          #x1511 #x6858
;;          #x8fa7 #x64f9
;;          #x4fa4 #xbefa))
;;
;;(define (hash-block-init-sha-256)
;;  (vector #xe667 #x6a09
;;          #xae85 #xbb67
;;          #xf372 #x3c6e
;;          #xf53a #xa54f
;;          #x527f #x510e
;;          #x688c #x9b05
;;          #xd9ab #x1f83
;;          #xcd19 #x5be0))
;;
;;(define-macro (fn-W dst i body)
;;  `(wref ,dst block ,i ,body))
;;
;;(define-macro (fn-R dst i body)
;;  `(wref T1 block ,(- i 15) ;; compute S0
;;   (wrot T2 T1 25
;;   (wrot T3 T1 14
;;   (wxor T4 T2 T3
;;   (wshr T5 T1 3
;;   (wxor S0 T4 T5
;;   (wref T6 block ,(- i 2) ;; compute S1
;;   (wrot T7 T6 15
;;   (wrot T8 T6 13
;;   (wxor T9 T7 T8
;;   (wshr T10 T6 10
;;   (wxor S1 T9 T10
;;   (wadd T11 S0 S1 ;; compute sum
;;   (wref T12 block ,(- i 7)
;;   (wadd T13 T11 T12
;;   (wref T14 block ,(- i 16)
;;   (wadd ,dst T13 T14
;;   (begin (wset block ,i ,dst)
;;   ,body)))))))))))))))))))
;;
;;(define-macro (fn-P a b c d e f g h x i k-hi16 k-lo16 body)
;;  `(wrot T1 ,a 30 ;; compute S2
;;   (wrot T2 ,a 19
;;   (wxor T3 T1 T2
;;   (wrot T4 ,a 10
;;   (wxor S2 T3 T4
;;   (wrot T5 ,e 26 ;; compute S3
;;   (wrot T6 ,e 21
;;   (wxor T7 T5 T6
;;   (wrot T8 ,e 7
;;   (wxor S3 T7 T8
;;   (wior T9 ,a ,b ;; compute F0
;;   (wand T10 ,c T9
;;   (wand T11 ,a ,b
;;   (wior F0 T10 T11
;;   (wxor T12 ,f ,g ;; compute F1
;;   (wand T13 ,e T12
;;   (wxor F1 ,g T13
;;   (,x T14 ,i ;; compute (fn-W i) or (fn-R i)
;;   (wadd T15 T14 S3 ;; compute sum
;;   (wadd T16 F1 T15
;;   (wlet T17 ,k-lo16 ,k-hi16
;;   (wadd T18 T16 T17
;;   (wadd T19 ,h T18
;;   (wadd T20 S2 F0
;;   (wadd ,d T19 ,d
;;   (wadd ,h T19 T20
;;   ,body)))))))))))))))))))))))))))
;;
;;(define (digest-update-sha-256 digest)
;;  (let* ((bd (digest-state digest))
;;         (hash (block-digest-hash bd))
;;         (block (block-digest-block bd)))
;;
;;    (define (stage1 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-W  0 #x428A #x2F98
;;      (fn-P H A B C D E F G fn-W  1 #x7137 #x4491
;;      (fn-P G H A B C D E F fn-W  2 #xB5C0 #xFBCF
;;      (fn-P F G H A B C D E fn-W  3 #xE9B5 #xDBA5
;;      (fn-P E F G H A B C D fn-W  4 #x3956 #xC25B
;;      (fn-P D E F G H A B C fn-W  5 #x59F1 #x11F1
;;      (fn-P C D E F G H A B fn-W  6 #x923F #x82A4
;;      (fn-P B C D E F G H A fn-W  7 #xAB1C #x5ED5
;;      (stage2 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage2 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-W  8 #xD807 #xAA98
;;      (fn-P H A B C D E F G fn-W  9 #x1283 #x5B01
;;      (fn-P G H A B C D E F fn-W 10 #x2431 #x85BE
;;      (fn-P F G H A B C D E fn-W 11 #x550C #x7DC3
;;      (fn-P E F G H A B C D fn-W 12 #x72BE #x5D74
;;      (fn-P D E F G H A B C fn-W 13 #x80DE #xB1FE
;;      (fn-P C D E F G H A B fn-W 14 #x9BDC #x06A7
;;      (fn-P B C D E F G H A fn-W 15 #xC19B #xF174
;;      (stage3 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage3 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 16 #xE49B #x69C1
;;      (fn-P H A B C D E F G fn-R 17 #xEFBE #x4786
;;      (fn-P G H A B C D E F fn-R 18 #x0FC1 #x9DC6
;;      (fn-P F G H A B C D E fn-R 19 #x240C #xA1CC
;;      (fn-P E F G H A B C D fn-R 20 #x2DE9 #x2C6F
;;      (fn-P D E F G H A B C fn-R 21 #x4A74 #x84AA
;;      (fn-P C D E F G H A B fn-R 22 #x5CB0 #xA9DC
;;      (fn-P B C D E F G H A fn-R 23 #x76F9 #x88DA
;;      (stage4 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage4 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 24 #x983E #x5152
;;      (fn-P H A B C D E F G fn-R 25 #xA831 #xC66D
;;      (fn-P G H A B C D E F fn-R 26 #xB003 #x27C8
;;      (fn-P F G H A B C D E fn-R 27 #xBF59 #x7FC7
;;      (fn-P E F G H A B C D fn-R 28 #xC6E0 #x0BF3
;;      (fn-P D E F G H A B C fn-R 29 #xD5A7 #x9147
;;      (fn-P C D E F G H A B fn-R 30 #x06CA #x6351
;;      (fn-P B C D E F G H A fn-R 31 #x1429 #x2967
;;      (stage5 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage5 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 32 #x27B7 #x0A85
;;      (fn-P H A B C D E F G fn-R 33 #x2E1B #x2138
;;      (fn-P G H A B C D E F fn-R 34 #x4D2C #x6DFC
;;      (fn-P F G H A B C D E fn-R 35 #x5338 #x0D13
;;      (fn-P E F G H A B C D fn-R 36 #x650A #x7354
;;      (fn-P D E F G H A B C fn-R 37 #x766A #x0ABB
;;      (fn-P C D E F G H A B fn-R 38 #x81C2 #xC92E
;;      (fn-P B C D E F G H A fn-R 39 #x9272 #x2C85
;;      (stage6 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage6 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 40 #xA2BF #xE8A1
;;      (fn-P H A B C D E F G fn-R 41 #xA81A #x664B
;;      (fn-P G H A B C D E F fn-R 42 #xC24B #x8B70
;;      (fn-P F G H A B C D E fn-R 43 #xC76C #x51A3
;;      (fn-P E F G H A B C D fn-R 44 #xD192 #xE819
;;      (fn-P D E F G H A B C fn-R 45 #xD699 #x0624
;;      (fn-P C D E F G H A B fn-R 46 #xF40E #x3585
;;      (fn-P B C D E F G H A fn-R 47 #x106A #xA070
;;      (stage7 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage7 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 48 #x19A4 #xC116
;;      (fn-P H A B C D E F G fn-R 49 #x1E37 #x6C08
;;      (fn-P G H A B C D E F fn-R 50 #x2748 #x774C
;;      (fn-P F G H A B C D E fn-R 51 #x34B0 #xBCB5
;;      (fn-P E F G H A B C D fn-R 52 #x391C #x0CB3
;;      (fn-P D E F G H A B C fn-R 53 #x4ED8 #xAA4A
;;      (fn-P C D E F G H A B fn-R 54 #x5B9C #xCA4F
;;      (fn-P B C D E F G H A fn-R 55 #x682E #x6FF3
;;      (stage8 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage8 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (fn-P A B C D E F G H fn-R 56 #x748F #x82EE
;;      (fn-P H A B C D E F G fn-R 57 #x78A5 #x636F
;;      (fn-P G H A B C D E F fn-R 58 #x84C8 #x7814
;;      (fn-P F G H A B C D E fn-R 59 #x8CC7 #x0208
;;      (fn-P E F G H A B C D fn-R 60 #x90BE #xFFFA
;;      (fn-P D E F G H A B C fn-R 61 #xA450 #x6CEB
;;      (fn-P C D E F G H A B fn-R 62 #xBEF9 #xA3F7
;;      (fn-P B C D E F G H A fn-R 63 #xC671 #x78F2
;;      (stage9 A-L A-H B-L B-H C-L C-H D-L D-H
;;              E-L E-H F-L F-H G-L G-H H-L H-H))))))))))
;;
;;    (define (stage9 A-L A-H B-L B-H C-L C-H D-L D-H
;;                    E-L E-H F-L F-H G-L G-H H-L H-H)
;;      (wref OLDA hash 0 (wadd NEWA A OLDA (wset hash 0 NEWA)))
;;      (wref OLDB hash 1 (wadd NEWB B OLDB (wset hash 1 NEWB)))
;;      (wref OLDC hash 2 (wadd NEWC C OLDC (wset hash 2 NEWC)))
;;      (wref OLDD hash 3 (wadd NEWD D OLDD (wset hash 3 NEWD)))
;;      (wref OLDE hash 4 (wadd NEWE E OLDE (wset hash 4 NEWE)))
;;      (wref OLDF hash 5 (wadd NEWF F OLDF (wset hash 5 NEWF)))
;;      (wref OLDG hash 6 (wadd NEWG G OLDG (wset hash 6 NEWG)))
;;      (wref OLDH hash 7 (wadd NEWH H OLDH (wset hash 7 NEWH))))
;;
;;    (wref A hash 0
;;    (wref B hash 1
;;    (wref C hash 2
;;    (wref D hash 3
;;    (wref E hash 4
;;    (wref F hash 5
;;    (wref G hash 6
;;    (wref H hash 7
;;    (stage1 A-L A-H B-L B-H C-L C-H D-L D-H
;;            E-L E-H F-L F-H G-L G-H H-L H-H)))))))))))
;;
;;(define (open-digest-sha-224)
;;  (make-digest
;;   end-block-digest
;;   digest-update-block-digest
;;   (make-block-digest
;;    digest-update-sha-256
;;    (hash-block-init-sha-224)
;;    (make-vector 160 0)
;;    0
;;    0
;;    #t
;;    224)))
;;
;;(define (open-digest-sha-256)
;;  (make-digest
;;   end-block-digest
;;   digest-update-block-digest
;;   (make-block-digest
;;    digest-update-sha-256
;;    (hash-block-init-sha-256)
;;    (make-vector 160 0)
;;    0
;;    0
;;    #t
;;    256)))

;;;----------------------------------------------------------------------------

(define (open-digest algorithm)

  ;; Dispatch on algorithm (can't use a "case" special form because on
  ;; case-insensitive systems this would cause duplicate cases).

  (cond ((memq algorithm '(crc32 CRC32))
         (open-digest-crc32))
        ((memq algorithm '(md5 MD5))
         (open-digest-md5))
;;        ((memq algorithm '(sha-1 SHA-1))
;;         (open-digest-sha-1))
;;        ((memq algorithm '(sha-224 SHA-224))
;;         (open-digest-sha-224))
;;        ((memq algorithm '(sha-256 SHA-256))
;;         (open-digest-sha-256))
;;        ((memq algorithm '(sha-384 SHA-384))
;;         (open-digest-sha-384))
;;        ((memq algorithm '(sha-512 SHA-512))
;;         (open-digest-sha-512))
        (else
         (error "unknown hashing algorithm" algorithm))))

;; (define-macro (digest-default-result-type) ''hex)
(define (digest-default-result-type)
  'hex)


(define (close-digest digest . maybe-result-type)
  (let ((result-type (if (null? maybe-result-type)
                         (digest-default-result-type)
                         (car maybe-result-type))))
    ((digest-end digest) digest result-type)))


(define (digest-update-subu8vector digest u8vect start end)
  ((digest-update digest) digest u8vect start end))

(define zero-u8vector (make-bytevector 4 0))

(define (get-zero-u8vector) zero-u8vector)

(define (digest-update-u8 digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (make-bytevector 1 (bitwise-and n #xff)))
   0
   1))

(define (digest-update-u16-le digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-bytevector 2)))
         (bytevector-u8-set!
          u8vect
          0
          (bitwise-and n #xff))
         (bytevector-u8-set!
          u8vect
          1
          (bitwise-and (arithmetic-shift n (- 8)) #xff))
         u8vect))
   0
   2))

(define (digest-update-u16-be digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-bytevector 2)))
         (bytevector-u8-set!
          u8vect
          1
          (bitwise-and n #xff))
         (bytevector-u8-set!
          u8vect
          0
          (bitwise-and (arithmetic-shift n (- 8)) #xff))
         u8vect))
   0
   2))

(define (digest-update-u32-le digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-bytevector 4)))
         (bytevector-u8-set!
          u8vect
          0
          (bitwise-and n #xff))
         (bytevector-u8-set!
          u8vect
          1
          (bitwise-and (arithmetic-shift n (- 8)) #xff))
         (bytevector-u8-set!
          u8vect
          2
          (bitwise-and (arithmetic-shift n (- 16)) #xff))
         (bytevector-u8-set!
          u8vect
          3
          (bitwise-and (arithmetic-shift n (- 24)) #xff))
         u8vect))
   0
   4))

(define (digest-update-u32-be digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-bytevector 4)))
         (bytevector-u8-set!
          u8vect
          3
          (bitwise-and n #xff))
         (bytevector-u8-set!
          u8vect
          2
          (bitwise-and (arithmetic-shift n (- 8)) #xff))
         (bytevector-u8-set!
          u8vect
          1
          (bitwise-and (arithmetic-shift n (- 16)) #xff))
         (bytevector-u8-set!
          u8vect
          0
          (bitwise-and (arithmetic-shift n (- 24)) #xff))
         u8vect))
   0
   4))


    (define (digest-string str algorithm . maybe-result-type)
      (let ((result-type (if (null? maybe-result-type)
                             (digest-default-result-type)
                             (car maybe-result-type))))
        (digest-substring
         str
         0
         (string-length str)
         algorithm
         result-type)))



    (define (digest-substring str start end algorithm . maybe-result-type)
      (let ((result-type (if (null? maybe-result-type)
                             (digest-default-result-type)
                             (car maybe-result-type))))
        (let* ((len (- end start))
               (u8vect (make-bytevector len)))
          (let loop ((i 0))
            (if (< i len)
                (begin
                  (bytevector-u8-set! u8vect i
                                      (char->integer (string-ref str i)))
                  (loop (+ i 1)))
                (digest-subu8vector u8vect 0 len algorithm result-type))))))



    (define (digest-u8vector u8vect algorithm . maybe-result-type)
      (let ((result-type (if (null? maybe-result-type)
                             (digest-default-result-type)
                             (car maybe-result-type))))
        (digest-subu8vector
         u8vect
         0
         (bytevector-length u8vect)
         algorithm
         result-type)))


    (define (digest-subu8vector u8vect start end algorithm . maybe-result-type)
      (let ((result-type (if (null? maybe-result-type)
                             (digest-default-result-type)
                             (car maybe-result-type))))
        (let ((digest (open-digest algorithm)))
          (digest-update-subu8vector digest u8vect start end)
          (close-digest digest result-type))))


(define (digest-file filename algorithm . maybe-result-type)
      (let ((result-type (if (null? maybe-result-type)
                             (digest-default-result-type)
                             (car maybe-result-type))))
        (let ((digest (open-digest algorithm)))
          (let* ((in (binio-open-input-file filename))
                 (bufsize 1024)
                 (buf (make-bytevector bufsize)))
            (let loop ()
              (let ((n (binio-read-subu8vector buf 0 bufsize in)))
                (if (= n 0)
                    (begin
                      (close-input-port in)
                      (close-digest digest result-type))
                    (begin
                      (digest-update-subu8vector digest buf 0 n)
                      (loop)))))))))


;;;----------------------------------------------------------------------------

))
