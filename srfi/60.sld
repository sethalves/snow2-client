(define-library (srfi 60)
  (export powers-of-two
          integer64->bitvector
          bitvector->integer64
          bitvector->str
          arithmetic-shift
          bitwise-ior
          bitwise-and
          bitwise-not
          bitwise-xor)
  (import (scheme base))
  (cond-expand
   (chibi (import (srfi 33)))
   (chicken (import (only (chicken)
                          arithmetic-shift
                          bitwise-ior
                          bitwise-and
                          bitwise-not
                          bitwise-xor)))
   ;; (gauche (import (srfi 60)))
   (sagittarius (import (rnrs arithmetic bitwise (6))))
   (else))
  (begin

    (define powers-of-two ;; 64 bits worth
      (vector
       #x8000000000000000 #x4000000000000000 #x2000000000000000 #x1000000000000000
       #x800000000000000 #x400000000000000 #x200000000000000 #x100000000000000
       #x80000000000000 #x40000000000000 #x20000000000000 #x10000000000000
       #x8000000000000 #x4000000000000 #x2000000000000 #x1000000000000

       #x800000000000 #x400000000000 #x200000000000 #x100000000000
       #x80000000000 #x40000000000 #x20000000000 #x10000000000
       #x8000000000 #x4000000000 #x2000000000 #x1000000000
       #x800000000 #x400000000 #x200000000 #x100000000

       #x80000000 #x40000000 #x20000000 #x10000000
       #x8000000 #x4000000 #x2000000 #x1000000
       #x800000 #x400000 #x200000 #x100000
       #x80000 #x40000 #x20000 #x10000

       #x8000 #x4000 #x2000 #x1000
       #x800 #x400 #x200 #x100
       #x80 #x40 #x20 #x10
       #x8 #x4 #x2 #x1))


    (define (integer64->bitvector v)
      ;; 64 bit big-endian, two's complement for negative
      (let ((bits (make-vector 64 #f)))
        (let loop ((i 0)
                   (v (if (< v 0) (+ #x10000000000000000 v) v)))
          (cond ((= i 64)
                 (if (= v 0)
                     bits
                     (error "integer64->bitvector -- v is too large")))
                ((<= (vector-ref powers-of-two i) v)
                 (vector-set! bits i #t)
                 (loop (+ i 1) (- v (vector-ref powers-of-two i))))
                (else
                 (loop (+ i 1) v))))))


    (define (bitvector->integer64 bits)
      ;; big-endian
      (let loop ((i 0)
                 (v 0))
        (cond ((= i 64)
               (if (>= v #x8000000000000000)
                   (- v #x10000000000000000)
                   v))
              ((vector-ref bits i)
               (loop (+ i 1) (+ v (vector-ref powers-of-two i))))
              (else
               (loop (+ i 1) v)))))


    (define (bitvector->str bits)
      (let loop ((i 0)
                 (result ""))
        (if (= i 64) result
            (loop (+ i 1)
                  (string-append result
                                 (if (vector-ref bits i) "1" "0"))))))


    (cond-expand

     ;; these have srfi-33 or srfi-60 available
     ((or chibi chicken gauche sagittarius))

     ;; (else
     ;;  (define (arithmetic-shift n sft)
     ;;    (if (> sft 0)
     ;;        (bit-lsh n sft)
     ;;        (bit-rsh n (- sft))))

     ;;  ;; (define bitwise-ior bit-or)
     ;;  (define (bitwise-ior . args)
     ;;    (if (null? args)
     ;;        0
     ;;        (bit-or (car args) (apply bitwise-ior (cdr args)))))

     ;;  (define bitwise-and bit-and)
     ;;  (define bitwise-not bit-not)
     ;;  (define (bitwise-xor . args)
     ;;    (let loop ((v (car args))
     ;;               (args (cdr args)))
     ;;      (if (null? args) v
     ;;          (loop (bit-xor v (car args))
     ;;                (cdr args))))))

     (else
      (define (bitwise-op v0 v1 op)
        (let ((v0-bits (integer64->bitvector v0))
              (v1-bits (integer64->bitvector v1))
              (bits (make-vector 64 #f)))
          (let loop ((i 0))
            (cond ((= i 64) (bitvector->integer64 bits))
                  ((op (vector-ref v0-bits i)
                       (vector-ref v1-bits i))
                   (vector-set! bits i #t)
                   (loop (+ i 1)))
                  (else
                   (loop (+ i 1)))))))


      (define (bitwise-ior . args)
        (let loop ((args args)
                   (result 0))
          (cond ((null? args) result)
                (else
                 (loop (cdr args)
                       (bitwise-op (car args) result
                                   ;; or
                                   (lambda (b0 b1)
                                     (cond (b0 #t)
                                           (b1 #t)
                                           (else #f)))))))))


      (define (bitwise-and . args)
        (let loop ((args args)
                   (result -1))
          (cond ((null? args) result)
                (else
                 (loop (cdr args)
                       (bitwise-op (car args) result
                                   ;; and
                                   (lambda (b0 b1)
                                     (cond ((not b0) #f)
                                           ((not b1) #f)
                                           (else #t)))))))))

      (define (bitwise-xor . args)
        (let loop ((args args)
                   (result 0))
          (cond ((null? args) result)
                (else
                 (loop (cdr args)
                       (bitwise-op (car args) result
                                   (lambda (b0 b1)
                                     (cond ((and b0 b1) #f)
                                           (b0 #t)
                                           (b1 #t)
                                           (else #f)))))))))

      (define (bitwise-not v)
        ;; (- 4294967295 v) 32 bit
        ;; (- #xffffffffffffffff v)
        (- -1 v))))


    (cond-expand
     ((or chicken chibi))
     (gauche
      (define bitwise-ior bitwise-ior)
      (define bitwise-xor bitwise-xor)
      (define arithmetic-shift arithmetic-shift))
     (sagittarius
      (define arithmetic-shift bitwise-arithmetic-shift))
     ;; (chibi
     ;;  ;; work around a bug?  difference?  in chibi's arithmetic-shift
     ;;  ;; https://code.google.com/p/chibi-scheme/issues/detail?id=208
     ;;  (define (arithmetic-shift v n)
     ;;    (cond ((< n 0)
     ;;           (floor (/ v (vector-ref powers-of-two
     ;;                                   (- (vector-length powers-of-two)
     ;;                                      (- 0 n) 1)))))
     ;;          ((> n 0)
     ;;           (* v (vector-ref powers-of-two
     ;;                            (- (vector-length powers-of-two) n 1))))
     ;;          (else v))
     ;;    ))
     (else
      ;; (define (arithmetic-shift v n)
      ;;   (cond ((= n 0) v)
      ;;         ((< n 0) (arithmetic-shift (floor (/ v 2)) (+ n 1)))
      ;;         ((> n 0) (arithmetic-shift (* v 2) (- n 1)))))
      (define (arithmetic-shift v n)
        (cond ((< n 0)
               (floor (/ v (vector-ref powers-of-two
                                       (- (vector-length powers-of-two)
                                          (- 0 n) 1)))))
              ((> n 0)
               (* v (vector-ref powers-of-two
                                (- (vector-length powers-of-two) n 1))))
              (else v))
        ;;   #xffffffffffffffff)
        )))
    ))
