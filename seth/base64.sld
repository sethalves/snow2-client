
;; http://en.wikipedia.org/wiki/Base64

(define-library (seth base64)
  (export encode decode
          encode-string decode-string
          encode-bytevector decode-bytevector
          integer->b64-char b64-char->integer
          scrub-encoded)
  (import (scheme base))
  (import (snow bytevector))
  (cond-expand
   (chibi
    (import (chibi base64) (chibi io)))
   (chicken
    (import (base64) (srfi 1) (srfi 4)))
   (foment (import (srfi 60)))
   (gauche
    (import (rfc base64) (gauche uvector)))
   (sagittarius
    (import (rfc base64)))
   )

  (begin
    (define integer->b64-char
      (list->vector
       (map char->integer
            '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
              #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
              #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
              #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
              #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))))

    (define b64-char->integer
      (vector
       ;; #t for whitespace, #f for illegal character, else an integer
       #f #f #f #f #f #f #f #f #t #f #t #f #f #t #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f
       #t ; space
       62 ; 33 !
       #f ; 34 "
       #f ; 35 #
       #f ; 36 $
       #f ; 37 %
       #f ; 38 &
       #f ; 39 '
       #f ; 40 (
       #f ; 41 )
       #f ; 42 *
       62 ; 43 +
       #f ; 44 ,
       63 ; 45 -
       63 ; 46 .
       63 ; 47 /
       52 ; 48 0
       53 ; 49 1
       54 ; 50 2
       55 ; 51 3
       56 ; 52 4
       57 ; 53 5
       58 ; 54 6
       59 ; 55 7
       60 ; 56 8
       61 ; 57 9
       63 ; 58 :
       #f ; 59 ;
       #f ; 60 <
       0 ; 61 =
       #f ; 62 >
       #f ; 63 ?
       #f ; 64 @
       0 ; 65 A
       1 ; 66 B
       2 ; 67 C
       3 ; 68 D
       4 ; 69 E
       5 ; 70 F
       6 ; 71 G
       7 ; 72 H
       8 ; 73 I
       9 ; 74 J
       10 ; 75 K
       11 ; 76 L
       12 ; 77 M
       13 ; 78 N
       14 ; 79 O
       15 ; 80 P
       16 ; 81 Q
       17 ; 82 R
       18 ; 83 S
       19 ; 84 T
       20 ; 85 U
       21 ; 86 V
       22 ; 87 W
       23 ; 88 X
       24 ; 89 Y
       25 ; 90 Z
       #f ; 91 [
       #f ; 92 \
       #f ; 93 ]
       #f ; 94 ^
       62 ; 95 _
       #f ; 96 `
       26 ; 97 a
       27 ; 98 b
       28 ; 99 c
       29 ; 100 d
       30 ; 101 e
       31 ; 102 f
       32 ; 103 g
       33 ; 104 h
       34 ; 105 i
       35 ; 106 j
       36 ; 107 k
       37 ; 108 l
       38 ; 109 m
       39 ; 110 n
       40 ; 111 o
       41 ; 112 p
       42 ; 113 q
       43 ; 114 r
       44 ; 115 s
       45 ; 116 t
       46 ; 117 u
       47 ; 118 v
       48 ; 119 w
       49 ; 120 x
       50 ; 121 y
       51 ; 122 z
       #f ; 123 {
       #f ; 124 |
       #f ; 125 }
       #f ; 126 ~
       #f ; 127 del
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f))

    (define (scrub-encoded in)
      ;; strip out whitespace and invalid characters.  return what's left
      ;; in a new bytevector.
      (let* ((in-len (bytevector-length in))
             (bogus-char-count
              (let loop ((bogus-char-count 0)
                         (pos 0))
                (if (= pos in-len)
                    bogus-char-count
                    (if (integer? (vector-ref b64-char->integer
                                              (bytevector-u8-ref in pos)))
                        (loop bogus-char-count (+ pos 1))
                        (loop (+ bogus-char-count 1) (+ pos 1))))))
             (out (make-bytevector (- in-len bogus-char-count))))
        (let loop ((in-pos 0)
                   (out-pos 0))
          (if (= in-pos in-len)
              out
              (let* ((in-v (bytevector-u8-ref in in-pos)))
                (if (integer? (vector-ref b64-char->integer in-v))
                    (begin
                      (bytevector-u8-set! out out-pos in-v)
                      (loop (+ in-pos 1) (+ out-pos 1)))
                    (loop (+ in-pos 1) out-pos)))))))



    (cond-expand


     (chibi
      (define (encode-bytevector in-bv)
        (string->utf8
         (base64-encode-string
          (utf8->string in-bv))))
      (define (decode-bytevector in-bv)
        (string->utf8
         (base64-decode-string
          (utf8->string in-bv))))
      (define encode-string base64-encode-string)
      (define decode-string base64-decode-string))


     (chicken
      (define (encode-bytevector in-bv)
        (string->utf8
         (base64-encode
          (utf8->string in-bv))))
      (define (decode-bytevector in-bv)
        (string->utf8
         (base64-decode
          (utf8->string in-bv))))
      (define (encode-string in-string)
        (base64-encode in-string))
      (define (decode-string in-string)
        (base64-decode in-string)))


     (gauche
      (define (encode-bytevector in-bv)
        (scrub-encoded
         (string->u8vector
          (base64-encode-string
           (u8vector->string in-bv)))))
      (define (decode-bytevector in-bv)
        (string->u8vector
         (base64-decode-string
          (u8vector->string in-bv))))
      (define (encode-string in-string)
        (u8vector->string
         (scrub-encoded
          (string->u8vector
           (base64-encode-string in-string)))))
      (define decode-string base64-decode-string))


     (sagittarius
      (define (encode-bytevector in-bv)
        (scrub-encoded
         (base64-encode in-bv)))
      (define decode-bytevector base64-decode)
      (define (encode-string in-string)
        (latin-1->string
         (scrub-encoded
          (string->latin-1
           (base64-encode-string in-string)))))
      (define decode-string base64-decode-string))


     (else
      (define (encode-bytevector in-bv)
        (define (in-bv-ref pos)
          (if (< pos (bytevector-length in-bv))
              (bytevector-u8-ref in-bv pos)
              0))
        (let* ((mod3 (modulo (bytevector-length in-bv) 3))
               (in-byte-count (+ (bytevector-length in-bv)
                                 (case mod3 ((0) 0) ((1) 2) ((2) 1))))
               (out-byte-count (/ (* in-byte-count 8) 6))
               (output (make-bytevector out-byte-count)))
          (let loop ((in-pos 0)
                     (out-pos 0))
            (if (>= in-pos in-byte-count) #t
                ;; 1111,11   11,1111,   1111,11   11,1111
                ;; #xfc0000 #x3f000 #xfc0 #x3f
                (let* ((tri-byte-val
                        (+ (arithmetic-shift (in-bv-ref in-pos) 16)
                           (arithmetic-shift (in-bv-ref (+ in-pos 1)) 8)
                           (in-bv-ref (+ in-pos 2))))
                       (i0 (arithmetic-shift
                            (bitwise-and #xfc0000 tri-byte-val) -18))
                       (i1 (arithmetic-shift
                            (bitwise-and #x3f000 tri-byte-val) -12))
                       (i2 (arithmetic-shift
                            (bitwise-and #xfc0 tri-byte-val) -6))
                       (i3 (bitwise-and #x3f tri-byte-val)))
                  (bytevector-u8-set! output out-pos
                                      (vector-ref integer->b64-char i0))
                  (bytevector-u8-set! output (+ out-pos 1)
                                      (vector-ref integer->b64-char i1))
                  (bytevector-u8-set! output (+ out-pos 2)
                                      (vector-ref integer->b64-char i2))
                  (bytevector-u8-set! output (+ out-pos 3)
                                      (vector-ref integer->b64-char i3))
                  (loop (+ in-pos 3)
                        (+ out-pos 4)))))

          ;; set last chars to #\=
          (let ((len (bytevector-length output)))
            (case mod3
              ((0) #t)
              ((1)
               (bytevector-u8-set! output (- len 1) 61)
               (bytevector-u8-set! output (- len 2) 61))
              ((2) (bytevector-u8-set! output (- len 1) 61))))

          output))


      (define (decode-bytevector in-bv)
        (define in-pos 0)
        (define (next-in)
          (if (< in-pos (bytevector-length in-bv))
              (let ((ret (bytevector-u8-ref in-bv in-pos)))
                (set! in-pos (+ in-pos 1))
                ret)
              0))
        (define (in-bv-ref pos)
          (if (and (>= pos 0) (< pos (bytevector-length in-bv)))
              (bytevector-u8-ref in-bv pos)
              0))

        (define output #f)
        (define out-pos 0)
        (define (next-out v)
          (if (< out-pos (bytevector-length output))
              (begin
                (bytevector-u8-set! output out-pos v)
                (set! out-pos (+ out-pos 1)))
              #f))
        (let* ((mod4 (modulo (bytevector-length in-bv) 4))
               (in-len (bytevector-length in-bv))
               (in-byte-count
                (+ in-len (case mod4 ((0) 0) ((1) 3) ((2) 2) ((3) 1))))
               (out-byte-count (/ (* in-byte-count 6) 8))
               (pad-count (+ (if (eqv? (in-bv-ref (- in-len 1)) 61) 1 0)
                             (if (eqv? (in-bv-ref (- in-len 2)) 61) 1 0))))
          (set! output (make-bytevector (- out-byte-count pad-count)))
          (let loop ()
            (if (>= in-pos in-byte-count) #t
                (let* ((c0 (next-in))
                       (c1 (next-in))
                       (c2 (next-in))
                       (c3 (next-in))
                       (n0 (vector-ref b64-char->integer c0))
                       (n1 (vector-ref b64-char->integer c1))
                       (n2 (vector-ref b64-char->integer c2))
                       (n3 (vector-ref b64-char->integer c3)))
                  (if (and (integer? n0)
                           (integer? n1)
                           (integer? n2)
                           (integer? n3))
                      (let* ((tri-byte-val (+ (arithmetic-shift n0 18)
                                              (arithmetic-shift n1 12)
                                              (arithmetic-shift n2 6)
                                              n3)))
                        (next-out
                         (arithmetic-shift
                          (bitwise-and tri-byte-val #xff0000) -16))
                        (next-out
                         (arithmetic-shift
                          (bitwise-and tri-byte-val #xff00) -8))
                        (next-out
                         (bitwise-and tri-byte-val #xff))
                        (loop))
                      (begin
                        ;; some bogus character
                        (loop))
                      ))))
          output))


      (define (encode-string in-string)
        (utf8->string
         (encode-bytevector
          (string->utf8 in-string))))

      (define (decode-string in-string)
        (utf8->string
         (decode-bytevector
          (string->utf8 in-string))))
      ))


    (define (encode input)
      (cond ((string? input) (encode-string input))
            ((bytevector? input) (encode-bytevector input))
            (else #f)))

    (define (decode input)
      (cond ((string? input) (decode-string input))
            ((bytevector? input) (decode-bytevector input))
            (else #f)))


    ;; (cond-expand (chicken (register-feature! 'seth.base64)) (else))
    ))
