(define-library (seth crypt sha-1)
  (export sha-1)
  (import (scheme base))
  (cond-expand
   (chibi (import (snow bytevector) (srfi 60)))
   (chicken (import (message-digest) (sha1)))
   (gauche (import (rfc sha) (snow bytevector)))
   (sagittarius (import (math hash) (snow bytevector))))
  (begin
    (cond-expand
     (chicken
      ;; http://wiki.call-cc.org/eggref/4/sha1
      (define (sha-1 src)
        (let ((in (cond ((string? src)
                         (open-input-bytevector (string->utf8 src)))
                        ((bytevector? src)
                         (open-input-bytevector src))
                        ((input-port? src) src)
                        (else (error "unknown digest source: " src)))))
          (message-digest-port (sha1-primitive) in 'u8vector))))

     (gauche
      (define (sha-1 src)
        ;; http://practical-scheme.net/gauche/man/gauche-refe_156.html
        (string->latin-1
         (cond ((string? src)
                (sha1-digest-string src))
               (else
                (let ((in (cond ((bytevector? src)
                                 (open-input-bytevector src))
                                ((input-port? src) src)
                                (else (error "unknown digest source: " src))))
                      (save-cip (current-input-port)))
                  (current-input-port in)
                  (let ((result (sha1-digest)))
                    (current-input-port save-cip)
                    result)))))))

     (sagittarius
      ;; http://ktakashi.github.io/sagittarius-ref.html#G2116
      (define (sha-1 src)
        (let ((in (cond ((string? src)
                         (open-input-bytevector (string->utf8 src)))
                        ((bytevector? src)
                         (open-input-bytevector src))
                        ((input-port? src) src)
                        (else (error "unknown digest source: " src))))
              (out (make-bytevector 20 0)))
          (let ((sha-1 (hash-algorithm SHA-1)))
            (hash-init! sha-1)
            (let loop ()
              (let ((bv (read-bytevector 1024 in)))
                (cond ((eof-object? bv)
                       (hash-done! sha-1 out)
                       out)
                      (else
                       (hash-process! sha-1 bv)
                       (loop)))))))))


     (else

      ;; http://www.ietf.org/rfc/rfc3174.txt

      (define (sha-1 src)
        (let ((in (cond ((string? src)
                         (open-input-bytevector (string->utf8 src)))
                        ((bytevector? src)
                         (open-input-bytevector src))
                        ((input-port? src) src)
                        (else (error "unknown digest source: " src))))
              (total-in-bytes 0)
              (done #f)
              (out (make-bytevector 20 0))
              (sha1-H0 #x67452301)
              (sha1-H1 #xefcdab89)
              (sha1-H2 #x98badcfe)
              (sha1-H3 #x10325476)
              (sha1-H4 #xc3d2e1f0))

          (define (+w . vals)
            (bitwise-and (apply + vals) #xffffffff))

          (define (left-rotate v n)
            (bitwise-ior
             (bitwise-and (arithmetic-shift v n) #xffffffff)
             (arithmetic-shift v (- (- 32 n)))))

          (define (sha1-block W)
            (define (sha1-f0 sha1-B sha1-C sha1-D)
              (bitwise-ior (bitwise-and sha1-B sha1-C)
                           (bitwise-and
                            (bitwise-xor sha1-B #xffffffff) sha1-D)))
            (define (sha1-f1 sha1-B sha1-C sha1-D)
              (bitwise-xor sha1-B sha1-C sha1-D))
            (define (sha1-f2 sha1-B sha1-C sha1-D)
              (bitwise-ior (bitwise-and sha1-B sha1-C)
                           (bitwise-and sha1-B sha1-D)
                           (bitwise-and sha1-C sha1-D)))
            ;; f(t;B,C,D) = (B AND C) OR ((NOT B) AND D)        ( 0 <= t <= 19)
            ;; f(t;B,C,D) = B XOR C XOR D                       (20 <= t <= 39)
            ;; f(t;B,C,D) = (B AND C) OR (B AND D) OR (C AND D) (40 <= t <= 59)
            ;; f(t;B,C,D) = B XOR C XOR D                       (60 <= t <= 79)
            (define (sha1-f t sha1-B sha1-C sha1-D)
              (cond ((< t 20) (sha1-f0 sha1-B sha1-C sha1-D))
                    ((< t 40) (sha1-f1 sha1-B sha1-C sha1-D))
                    ((< t 60) (sha1-f2 sha1-B sha1-C sha1-D))
                    (else (sha1-f1 sha1-B sha1-C sha1-D))))

            (define (sha1-K t)
              (cond ((< t 20) #x5a827999)
                    ((< t 40) #x6ed9eba1)
                    ((< t 60) #x8f1bbcdc)
                    (else #xca62c1d6)))

            ;; b. For t = 16 to 79 let
            ;;    W(t) = S^1( W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16) ).
            (let loop ((t 16))
              (if (= t 80) #t
                  (begin
                    (vector-set! W t
                                 (left-rotate
                                  (bitwise-xor (vector-ref W (- t 3))
                                               (vector-ref W (- t 8))
                                               (vector-ref W (- t 14))
                                               (vector-ref W (- t 16))) 1))
                    (loop (+ t 1)))))

            ;; c. Let A = H0, B = H1, C = H2, D = H3, E = H4.
            (let loop ((sha1-A sha1-H0)
                       (sha1-B sha1-H1)
                       (sha1-C sha1-H2)
                       (sha1-D sha1-H3)
                       (sha1-E sha1-H4)
                       (t 0))
              ;; d. For t = 0 to 79 do
              ;;    TEMP = S^5(A) + f(t;B,C,D) + E + W(t) + K(t);
              ;;    E = D;  D = C;  C = S^30(B);  B = A; A = TEMP;
              (if (< t 80)
                  (let* ((temp (+w (left-rotate sha1-A 5)
                                   (sha1-f t sha1-B sha1-C sha1-D)
                                   sha1-E
                                   (vector-ref W t)
                                   (sha1-K t))))
                    (loop temp
                          sha1-A
                          (left-rotate sha1-B 30)
                          sha1-C
                          sha1-D
                          (+ t 1)))
                  (begin
                    ;; done.  do step e.
                    (set! sha1-H0 (+w sha1-H0 sha1-A))     ;; Let H0 = H0 + A,
                    (set! sha1-H1 (+w sha1-H1 sha1-B))     ;;     H1 = H1 + B,
                    (set! sha1-H2 (+w sha1-H2 sha1-C))     ;;     H2 = H2 + C,
                    (set! sha1-H3 (+w sha1-H3 sha1-D))     ;;     H3 = H3 + D,
                    (set! sha1-H4 (+w sha1-H4 sha1-E)))))) ;;     H4 = H4 + E


          (define get-next-byte
            (let ((sent-80 #f))
              (lambda ()
                (cond (sent-80 (eof-object))
                      (else
                       (let ((b (read-u8 in)))
                         (cond ((eof-object? b)
                                (set! sent-80 #t)
                                #x80)
                               (else
                                (set! total-in-bytes (+ total-in-bytes 1))
                                b))))))))


          (define (get-next-4-bytes)
            (let* ((b0 (get-next-byte))
                   (b1 (get-next-byte))
                   (b2 (get-next-byte))
                   (b3 (get-next-byte)))
              (cond ((eof-object? b0) b0)
                    (else
                     (let ((b1 (if (eof-object? b1) 0 b1))
                           (b2 (if (eof-object? b2) 0 b2))
                           (b3 (if (eof-object? b3) 0 b3)))
                       (+ (arithmetic-shift b0 24)
                          (arithmetic-shift b1 16)
                          (arithmetic-shift b2 8)
                          b3))))))

          (define (store-length W)
            (let ((bits (* total-in-bytes 8)))
              (vector-set!
               W 14
               (arithmetic-shift (bitwise-and bits #xffffffff00000000) -32))
              (vector-set! W 15 (bitwise-and bits #xffffffff))))



          (define (byte-extract n byte-num)
            (case byte-num
              ((0) (arithmetic-shift (bitwise-and n #xff000000) -24))
              ((1) (arithmetic-shift (bitwise-and n #xff0000) -16))
              ((2) (arithmetic-shift (bitwise-and n #xff00) -8))
              ((3) (bitwise-and n #xff))))


          (let loop ()
            (let ((W (make-vector 80 0)))
              (let W-loop ((i 0))
                (cond ((= i 16)
                       (sha1-block W)
                       (loop))
                      (else
                       (let ((v (get-next-4-bytes)))
                         (cond ((and (eof-object? v) (< i 15))
                                (store-length W)
                                (sha1-block W))
                               ((eof-object? v)
                                ;; there wasn't room for the length in the
                                ;; last block, so process this block
                                ;; and go around one more time.
                                (sha1-block W)
                                (loop))
                               (else
                                (vector-set! W i v)
                                (W-loop (+ i 1))))))))))

          (let ((result (make-bytevector 20)))
            (bytevector-u8-set! result 0 (byte-extract sha1-H0 0))
            (bytevector-u8-set! result 1 (byte-extract sha1-H0 1))
            (bytevector-u8-set! result 2 (byte-extract sha1-H0 2))
            (bytevector-u8-set! result 3 (byte-extract sha1-H0 3))

            (bytevector-u8-set! result 4 (byte-extract sha1-H1 0))
            (bytevector-u8-set! result 5 (byte-extract sha1-H1 1))
            (bytevector-u8-set! result 6 (byte-extract sha1-H1 2))
            (bytevector-u8-set! result 7 (byte-extract sha1-H1 3))

            (bytevector-u8-set! result 8 (byte-extract sha1-H2 0))
            (bytevector-u8-set! result 9 (byte-extract sha1-H2 1))
            (bytevector-u8-set! result 10 (byte-extract sha1-H2 2))
            (bytevector-u8-set! result 11 (byte-extract sha1-H2 3))

            (bytevector-u8-set! result 12 (byte-extract sha1-H3 0))
            (bytevector-u8-set! result 13 (byte-extract sha1-H3 1))
            (bytevector-u8-set! result 14 (byte-extract sha1-H3 2))
            (bytevector-u8-set! result 15 (byte-extract sha1-H3 3))

            (bytevector-u8-set! result 16 (byte-extract sha1-H4 0))
            (bytevector-u8-set! result 17 (byte-extract sha1-H4 1))
            (bytevector-u8-set! result 18 (byte-extract sha1-H4 2))
            (bytevector-u8-set! result 19 (byte-extract sha1-H4 3))

            result)))))))
