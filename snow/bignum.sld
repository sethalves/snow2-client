;;;============================================================================

;;; File: "bignum.scm", Time-stamp: <2007-04-05 00:50:01 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides operations on bignums.  The implementation relies only
;;; on operations on fixnums.


(define-library (snow bignum)
  (export bignum=
          bignum<
          bignum>
          bignum<=
          bignum>=
          bignum-zero?
          bignum-negative?
          bignum-positive?
          bignum-even?
          bignum-odd?
          bignum+
          bignum-
          bignum*
          bignum-quotient
          bignum-remainder
          bignum-modulo
          bignum-max
          bignum-min
          bignum-abs
          bignum-gcd
          bignum-lcm
          bignum-expt
          bignum-expt-mod
          bignum-bitwise-not
          bignum-integer-length
          bignum->string
          string->bignum
          bignum->fixnum
          fixnum->bignum
          bignum->fixnum-list
          fixnum-list->bignum
          bignum->u8vector
          u8vector->bignum
;          bignum->base64-string
;          base64-string->bignum
          )
  (import (scheme base)
          (scheme char)
          (snow bytevector)
;          (snow base64)
          )
  (begin


;;;============================================================================

    (cond-expand

     (bigloo

      ;; Bignums are represented with vectors of "bignum digits":
      ;;
      ;; assuming that the non-negative bignum "n" is represented by
      ;; the vector "v" of length "k", we have
      ;;
      ;;                       k-2
      ;;                      -----
      ;;                      \                   i
      ;; n  =  (v[0]*2-1)  *   >   v[i+1] * radix
      ;;                      /
      ;;                      -----
      ;;                      i = 0
      ;;
      ;; Moreover, for all n, v[k-1] != 0.
      ;;
      ;; note: v[0] = 0 if n is negative, v[0] = 1 if n is non-negative.
      ;;
      ;; "radix" must be less than or equal to sqrt(max fixnum)+1.  This
      ;; guarantees that the result of an arithmetic operation on bignum
      ;; digits will be a fixnum (this includes the product of two digits).

      (define (bignum-radix-width)   14)
      (define (bignum-radix)         (expt 2 14))
      (define (bignum-radix-minus-1) (- (expt 2 14) 1))

      (cond-expand

       (chicken

        ;; Chicken has 31 bit fixnums

        (define (bignum-max-fixnum)
          (- -1 (* -2 (expt 2 29))))

        (define (bignum-min-fixnum)
          (* -2 (expt 2 29)))

        (define (bignum-min-fixnum-div-radix)
          (quotient (* -2 (expt 2 29)) 16384)))

       (else

        ;; Bigloo and most others have 30 bit fixnums

        (define (bignum-max-fixnum)
          (- -1 (* -2 (expt 2 28))))

        (define (bignum-min-fixnum)
          (* -2 (expt 2 28)))

        (define (bignum-min-fixnum-div-radix)
          (quotient (* -2 (expt 2 28)) 16384))))

      (define (make-bignum len)              (make-u16-bytevector len 0))
      (define (bignum-length bn)             (bytevector-u16-length bn))
      (define (bignum-sign bn)               (bytevector-u16-ref bn 0))
      (define (bignum-sign-set! bn sign)     (bytevector-u16-set! bn 0 sign))
      (define (bignum-digit-ref bn i)        (bytevector-u16-ref bn i))
      (define (bignum-digit-set! bn i digit) (bytevector-u16-set! bn i digit))
      (define (bignum-set-neg! bn)           (bignum-sign-set! bn 0))
      (define (bignum-set-nonneg! bn)        (bignum-sign-set! bn 1))

      ;;---------------------------------------------------------------------------

      ;; Utilities

      (define (bignum-shrink x len)
        (let ((y (make-bignum len)))
          (let loop ((i (- len 1)))
            (if (< i 0)
                y
                (begin
                  (bignum-digit-set! y i (bignum-digit-ref x i))
                  (loop (- i 1)))))))

      (define (bignum-remove-leading-zeroes x)
        (let ((sign (bignum-sign x)))
          (bignum-set-nonneg! x) ;; setup sentinel (sign = anything != 0)
          (let loop ((i (- (bignum-length x) 1)))
            (cond ((= (bignum-digit-ref x i) 0)
                   (loop (- i 1)))
                  ((= i 0)
                   bignum-zero)
                  (else
                   (bignum-sign-set! x sign)
                   (if (= i (- (bignum-length x) 1))
                       x
                       (bignum-shrink x (+ i 1))))))))

      (define (bignum-normalize x)
        ;; (or (bignum->fixnum x) x)
        x)

      (define (fixnum->bignum-fresh n)
        (let ((neg-n (if (< n 0) n (- 0 n))))
          ;; computing with negative n avoids overflow
          (let loop1 ((nb-digits 0) (x neg-n))
            (if (not (= x 0))
                (loop1 (+ nb-digits 1) (quotient x (bignum-radix)))
                (let ((r (make-bignum (+ nb-digits 1))))
                  (if (< n 0)
                      (bignum-set-neg! r)
                      (bignum-set-nonneg! r))
                  (let loop2 ((i 1) (x neg-n))
                    (if (not (= x 0))
                        (begin
                          (bignum-digit-set!
                           r
                           i
                           (- 0 (remainder x (bignum-radix))))
                          (loop2 (+ i 1) (quotient x (bignum-radix))))
                        r)))))))

      (define preallocated-bignums
        (let ((v (make-vector 33 #f)))
          (let loop ((i 0) (n -16))
            (if (< 16 n)
                v
                (begin
                  (vector-set! v i (fixnum->bignum-fresh n))
                  (loop (+ i 1) (+ n 1)))))))

      (define (fixnum->bignum n)
        (if (or (< n -16) (< 16 n))
            (fixnum->bignum-fresh n)
            (vector-ref preallocated-bignums (+ n 16))))

      (define bignum-zero
        (fixnum->bignum 0))

      ;;---------------------------------------------------------------------------

      ;; Bignum comparison

      (define (bignum= x y)
        (if (not (= (bignum-sign x) (bignum-sign y)))
            #f
            (let ((lenx (bignum-length x)))
              (if (not (= lenx (bignum-length y)))
                  #f
                  (let loop ((i (- lenx 1)))
                    (if (< 0 i)
                        (if (not (= (bignum-digit-ref x i)
                                    (bignum-digit-ref y i)))
                            #f
                            (loop (- i 1)))
                        #t))))))

      (define (bignum< x y)
        (if (not (= (bignum-sign x) (bignum-sign y)))
            (bignum-negative? x)
            (let ((lenx (bignum-length x))
                  (leny (bignum-length y)))
              (cond ((< lenx leny)
                     (not (bignum-negative? x)))
                    ((< leny lenx)
                     (bignum-negative? x))
                    (else
                     (let loop ((i (- lenx 1)))
                       (if (< 0 i)
                           (let ((dx (bignum-digit-ref x i))
                                 (dy (bignum-digit-ref y i)))
                             (cond ((< dx dy) (not (bignum-negative? x)))
                                   ((< dy dx) (bignum-negative? x))
                                   (else      (loop (- i 1)))))
                           #f)))))))

      (define (bignum> x y)
        (bignum< y x))

      (define (bignum<= x y)
        (not (bignum< y x)))

      (define (bignum>= x y)
        (not (bignum< x y)))

      (define (bignum-zero? x)
        (= (bignum-length x) 1))

      (define (bignum-negative? x)
        (= (bignum-sign x) 0))

      (define (bignum-positive? x)
        (not (or (bignum-zero? x)
                 (bignum-negative? x))))

      (define (bignum-even? x)
        (or (bignum-zero? x)
            (even? (bignum-digit-ref x 1))))

      (define (bignum-odd? x)
        (not (bignum-even? x)))

      ;;---------------------------------------------------------------------------

      ;; Bignum addition and substraction

      (define (bignum-add-nonneg x y)

        (define (add x y lenx leny)
          (let ((r (make-bignum (+ lenx 1))))

            (bignum-set-nonneg! r)

            (let loop1 ((i 1) (c 0)) ;; add digits in y
              (if (< i leny)

                  (let ((w (+ (+ (bignum-digit-ref x i)
                                 (bignum-digit-ref y i))
                              c)))
                    (if (< w (bignum-radix))
                        (begin
                          (bignum-digit-set! r i w)
                          (loop1 (+ i 1) 0))
                        (begin
                          (bignum-digit-set! r i (- w (bignum-radix)))
                          (loop1 (+ i 1) 1))))

                  (let loop2 ((i i) (c c)) ;; propagate carry
                    (if (< i lenx)

                        (let ((w (+ (bignum-digit-ref x i) c)))
                          (if (< w (bignum-radix))
                              (begin
                                (bignum-digit-set! r i w)
                                (loop2 (+ i 1) 0))
                              (begin
                                (bignum-digit-set! r i (- w (bignum-radix)))
                                (loop2 (+ i 1) 1))))

                        (if (= c 0)
                            (bignum-shrink r lenx)
                            (begin
                              (bignum-digit-set! r lenx c)
                              r))))))))

        (let ((lenx (bignum-length x))
              (leny (bignum-length y)))
          (if (< lenx leny)
              (add y x leny lenx)
              (add x y lenx leny))))

      (define (bignum-sub-nonneg x y)

        (define (complement! r)
          (let ((lr (bignum-length r)))
            (let loop ((i 1) (c 0))
              (if (< i lr)

                  (let ((w (+ (bignum-digit-ref r i) c)))
                    (if (< 0 w)
                        (begin
                          (bignum-digit-set! r i (- (bignum-radix) w))
                          (loop (+ i 1) 1))
                        (begin
                          (bignum-digit-set! r i 0)
                          (loop (+ i 1) 0))))))))

        (define (sub x y lenx leny)
          (let ((r (make-bignum lenx)))

            (let loop1 ((i 1) (b 0)) ;; substract digits in y
              (if (< i leny)

                  (let ((w (- (- (bignum-digit-ref x i)
                                 (bignum-digit-ref y i))
                              b)))
                    (if (< w 0)
                        (begin
                          (bignum-digit-set! r i (+ w (bignum-radix)))
                          (loop1 (+ i 1) 1))
                        (begin
                          (bignum-digit-set! r i w)
                          (loop1 (+ i 1) 0))))

                  (let loop2 ((i i) (b b)) ;; propagate borrow
                    (if (< i lenx)

                        (let ((w (- (bignum-digit-ref x i) b)))
                          (if (< w 0)
                              (begin
                                (bignum-digit-set! r i (+ w (bignum-radix)))
                                (loop2 (+ i 1) 1))
                              (begin
                                (bignum-digit-set! r i w)
                                (loop2 (+ i 1) 0))))

                        (if (= b 0)
                            (bignum-set-nonneg! r)
                            (begin
                              (bignum-set-neg! r)
                              (complement! r)))))))

            (bignum-remove-leading-zeroes r)))
        
        (sub x y (bignum-length x) (bignum-length y)))

      (define (bignum-sum2 x y sign-x sign-y)

        (define (adjust-sign x s)
          (if (= (bignum-sign x) s)
              (bignum-set-nonneg! x)
              (bignum-set-neg! x))
          (bignum-normalize (bignum-remove-leading-zeroes x)))

        (cond ((= sign-x sign-y) ;; same sign
               (adjust-sign (bignum-add-nonneg x y) sign-x))
              ((< (bignum-length x) (bignum-length y))
               (adjust-sign (bignum-sub-nonneg y x) sign-y))
              (else
               (adjust-sign (bignum-sub-nonneg x y) sign-x))))

      (define (bignum-add2 x y)
        (bignum-sum2 x y (bignum-sign x) (bignum-sign y)))

      (define (bignum-sub2 x y)
        (bignum-sum2 x y (bignum-sign x) (- 1 (bignum-sign y))))

      (define (bignum-negate x)
        (bignum-sub2 bignum-zero x))

      (define (bignum+ . args)
        (if (pair? args)
            (let loop ((n (car args)) (lst (cdr args)))
              (if (pair? lst)
                  (loop (bignum-add2 n (car lst)) (cdr lst))
                  n))
            bignum-zero))

      (define (bignum- x . args)
        (if (pair? args)
            (let loop ((n x) (lst args))
              (if (pair? lst)
                  (loop (bignum-sub2 n (car lst)) (cdr lst))
                  n))
            (bignum-negate x)))

      ;;---------------------------------------------------------------------------

      ;; Bignum multiplication

      (define (bignum-mul2 x y)

        (define (mul x y lenx leny)
          (let ((r (make-bignum (- (+ lenx leny) 1))))

            (if (= (bignum-sign x) (bignum-sign y))
                (bignum-set-nonneg! r)
                (bignum-set-neg! r))

            (let loop1 ((j 1)) ;; for each digit in y
              (if (< j leny)

                  (let ((d (bignum-digit-ref y j)))
                    (if (= d 0) ;; useful optimization for powers of 2
                        (loop1 (+ j 1))
                        (let loop2 ((i 1) (k j) (c 0)) ;; multiply and add
                          (if (< i lenx)

                              (let ((w (+ (+ (bignum-digit-ref r k) c)
                                          (* (bignum-digit-ref x i) d))))
                                (bignum-digit-set! r k (modulo w (bignum-radix)))
                                (loop2 (+ i 1)
                                       (+ k 1)
                                       (quotient w (bignum-radix))))

                              (begin
                                (bignum-digit-set! r k c)
                                (loop1 (+ j 1)))))))))

            (bignum-remove-leading-zeroes r)))

        (bignum-normalize (mul x y (bignum-length x) (bignum-length y))))

      (define (bignum* . args)
        (if (pair? args)
            (let loop ((n (car args)) (lst (cdr args)))
              (if (pair? lst)
                  (loop (bignum-mul2 n (car lst)) (cdr lst))
                  n))
            (fixnum->bignum 1)))

      ;;---------------------------------------------------------------------------

      ;; Bignum division

      (define (bignum-div x y)

        (define (single-digit-divisor-div x y lenx leny r)

          ;; simple algo for single digit divisor

          (let ((d (bignum-digit-ref y 1)))
            (let loop1 ((i (- lenx 1)) (k 0))
              (if (< 0 i)
                  (let ((w (+ (* k (bignum-radix)) (bignum-digit-ref x i))))
                    (bignum-digit-set! r i (quotient w d))
                    (loop1 (- i 1) (remainder w d)))
                  (cons (bignum-remove-leading-zeroes r)
                        (fixnum->bignum
                         (if (bignum-negative? x) (- 0 k) k)))))))

        (define (multi-digit-divisor-div x y lenx leny r)

          ;; general algo from Knuth

          ;; STEP 1: normalize x and y

          (let loop2 ((shift 1)
                      (n (* (bignum-digit-ref y (- leny 1)) 2)))
            (if (< n (bignum-radix))
                (loop2 (* shift 2) (* n 2))

                (let ((nx (make-bignum (+ lenx 1)))
                      (ny (make-bignum leny)))

                  (bignum-sign-set! nx (bignum-sign x))

                  (let loop3 ((i 1) (c 0))
                    (if (< i lenx)
                        (let ((w (+ (* (bignum-digit-ref x i) shift) c)))
                          (bignum-digit-set! nx i (modulo w (bignum-radix)))
                          (loop3 (+ i 1) (quotient w (bignum-radix))))
                        (bignum-digit-set! nx i c)))

                  (let loop4 ((i 1) (c 0))
                    (if (< i leny)
                        (let ((w (+ (* (bignum-digit-ref y i) shift) c)))
                          (bignum-digit-set! ny i (modulo w (bignum-radix)))
                          (loop4 (+ i 1) (quotient w (bignum-radix))))))

                  (let loop5 ((i lenx))
                    (if (not (< i leny))

                        ;; STEP 2: calculate next digit in quotient

                        (let ((msd-of-ny
                               (bignum-digit-ref ny (- leny 1)))
                              (next-msd-of-ny
                               (bignum-digit-ref ny (- leny 2)))
                              (msd-of-nx
                               (bignum-digit-ref nx i))
                              (next-msd-of-nx
                               (bignum-digit-ref nx (- i 1)))
                              (next-next-msd-of-nx
                               (bignum-digit-ref nx (- i 2))))

                          (define (next-digit q u)
                            (if (< u (bignum-radix))
                                (let* ((temp1 (* q next-msd-of-ny))
                                       (temp2 (quotient temp1 (bignum-radix))))
                                  (if (or (< u temp2)
                                          (and (= temp2 u)
                                               (< next-next-msd-of-nx
                                                  (remainder temp1 (bignum-radix)))))
                                      (next-digit (- q 1) (+ u msd-of-ny))
                                      q))
                                q))

                          (let ((q (if (= msd-of-nx msd-of-ny)
                                       (next-digit (bignum-radix-minus-1)
                                                   (+ msd-of-ny next-msd-of-nx))
                                       (let ((temp (+ (* msd-of-nx (bignum-radix))
                                                      next-msd-of-nx)))
                                         (next-digit (quotient temp msd-of-ny)
                                                     (modulo temp msd-of-ny))))))

                            ;; STEP 3: multiply and substract

                            (let loop7 ((j 1)
                                        (k (- i (- leny 1)))
                                        (b 0))
                              (if (< j leny)

                                  (let ((w (- (+ (bignum-digit-ref nx k) b)
                                              (* (bignum-digit-ref ny j) q))))
                                    (bignum-digit-set!
                                     nx
                                     k
                                     (modulo w (bignum-radix)))
                                    (loop7 (+ j 1)
                                           (+ k 1)
                                           (quotient (- w (bignum-radix-minus-1))
                                                     (bignum-radix))))

                                  (let ((w (+ (bignum-digit-ref nx k) b)))
                                    (bignum-digit-set!
                                     nx
                                     k
                                     (modulo w (bignum-radix)))
                                    (if (< w 0)
                                        (begin
                                          (bignum-digit-set!
                                           r
                                           (- i (- leny 1))
                                           (- q 1))
                                          (let loop8 ((j 1)
                                                      (k (- i (- leny 1)))
                                                      (c 0))
                                            (if (< j leny)

                                                (let ((w
                                                       (+
                                                        (+ (bignum-digit-ref nx k)
                                                           (bignum-digit-ref ny j))
                                                        c)))
                                                  (bignum-digit-set!
                                                   nx
                                                   k
                                                   (modulo w (bignum-radix)))
                                                  (loop8
                                                   (+ j 1)
                                                   (+ k 1)
                                                   (quotient w (bignum-radix))))
                                                (bignum-digit-set!
                                                 nx
                                                 k
                                                 (modulo
                                                  (+ (bignum-digit-ref nx k) c)
                                                  (bignum-radix))))))
                                        (bignum-digit-set! r (- i (- leny 1)) q))
                                    (loop5 (- i 1)))))))))

                  (let loop9 ((i (- leny 1)) (k 0))
                    (if (< 0 i)
                        (let ((w (+ (* k (bignum-radix))
                                    (bignum-digit-ref nx i))))
                          (bignum-digit-set! nx i (quotient w shift))
                          (loop9 (- i 1)
                                 (remainder w shift)))))

                  (cons (bignum-remove-leading-zeroes r)
                        (bignum-remove-leading-zeroes nx))))))

        (define (div x y lenx leny)
          (if (< lenx leny)

              (cons bignum-zero x)

              (let ((r (make-bignum (+ (- lenx leny) 2))))

                (if (= (bignum-sign x) (bignum-sign y))
                    (bignum-set-nonneg! r)
                    (bignum-set-neg! r))

                (if (= leny 2)
                    (single-digit-divisor-div x y lenx leny r)
                    (multi-digit-divisor-div x y lenx leny r)))))

        (if (bignum-zero? y)
            (error "divide by zero")
            (div x y (bignum-length x) (bignum-length y))))

      (define (bignum-quotient x y)
        (bignum-normalize (car (bignum-div x y))))

      (define (bignum-remainder x y)
        (bignum-normalize (cdr (bignum-div x y))))

      (define (bignum-modulo x y)
        (let ((r (cdr (bignum-div x y))))
          (if (or (bignum-zero? r)
                  (eqv? (bignum-negative? x)
                        (bignum-negative? y)))
              (bignum-normalize r)
              (bignum+ r y))))

      ;;---------------------------------------------------------------------------

      ;; Bignum MAX, MIN, ABS, GCD and LCM

      (define (bignum-max2 x y)
        (if (bignum< x y) y x))

      (define (bignum-max x . args)
        (let loop ((n x) (lst args))
          (if (pair? lst)
              (loop (bignum-max2 n (car lst)) (cdr lst))
              n)))

      (define (bignum-min2 x y)
        (if (bignum< x y) x y))

      (define (bignum-min x . args)
        (let loop ((n x) (lst args))
          (if (pair? lst)
              (loop (bignum-min2 n (car lst)) (cdr lst))
              n)))

      (define (bignum-abs x)
        (if (bignum-negative? x)
            (bignum-negate x)
            x))

      (define (bignum-gcd x y)
        (let loop ((x (bignum-abs x)) (y (bignum-abs y)))
          (if (bignum-zero? y)
              x
              (loop y (bignum-remainder x y)))))

      (define (bignum-lcm x y)
        (if (or (bignum-zero? x) (bignum-zero? y))
            bignum-zero
            (bignum-quotient
             (bignum-abs (bignum-mul2 x y))
             (bignum-gcd x y))))

      ;;---------------------------------------------------------------------------

      ;; Bignum exponentiation

      (define (bignum-expt x y)
        (cond ((bignum-zero? y)
               (fixnum->bignum 1))
              ((bignum-even? y)
               (bignum-expt
                (bignum-mul2 x x)
                (bignum-quotient y (fixnum->bignum 2))))
              (else
               (bignum-mul2
                x
                (bignum-expt
                 x
                 (bignum- y (fixnum->bignum 1)))))))

      ;;---------------------------------------------------------------------------

      ;; Bitwise operations

      (define (bignum-bitwise-not x)
        (bignum-sub2 (fixnum->bignum -1) x))

      (define (bignum-integer-length x)
        (let* ((n (if (bignum-negative? x) (bignum-bitwise-not x) x))
               (nb-digits (- (bignum-length n) 1)))
          (if (= nb-digits 0)
              0
              (let loop ((d (bignum-digit-ref n nb-digits))
                         (len (* (- nb-digits 1) (bignum-radix-width))))
                (if (< 0 d)
                    (loop (quotient d 2)
                          (+ len 1))
                    len)))))

      ;;---------------------------------------------------------------------------

      ;; Conversion to and from string

      ;; (define* (bignum->string x (radix 10)) ;; 2 <= radix <= 36
      (define (bignum->string x . maybe-radix) ;; 2 <= radix <= 36
        (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
          (define (digit->char d)
            (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" d))

          (define (convert-non-neg sign n)
            (let ((digits
                   (map digit->char
                        (if (bignum-zero? n)
                            '(0)
                            (reverse (bignum->fixnum-list n (- radix 1)))))))
              (list->string (if sign (cons sign digits) digits))))

          (if (bignum-negative? x)
              (convert-non-neg #\- (bignum-negate x))
              (convert-non-neg #f x))))

      ;; (define* (string->bignum str (radix 10)) ;; 2 <= radix <= 36
      (define (string->bignum str . maybe-radix)
        (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
          (define (char->digit c rad)

            (define (check d)
              (if (< d rad)
                  d
                  #f))

            (cond ((and (char>=? c #\0) (char<=? c #\9))
                   (check (- (char->integer c) (char->integer #\0))))
                  ((and (char>=? c #\a) (char<=? c #\z))
                   (check (+ 10 (- (char->integer c) (char->integer #\a)))))
                  ((and (char>=? c #\A) (char<=? c #\Z))
                   (check (+ 10 (- (char->integer c) (char->integer #\A)))))
                  (else
                   #f)))

          (define (convert rad sign i)
            (if (<= (+ i 1) (string-length str)) ;; need at least one digit
                (let loop ((i i) (digits '()))
                  (if (< i (string-length str))
                      (let ((d (char->digit (string-ref str i) rad)))
                        (if d
                            (loop (+ i 1) (cons d digits))
                            #f))
                      (let ((n (fixnum-list->bignum digits (- rad 1))))
                        (if (and sign (char=? sign #\-))
                            (bignum-negate n)
                            n))))
                #f))

          (define (sign-prefix rad i)
            (cond ((and (<= (+ i 2) (string-length str)) ;; need at least two chars
                        (memv (string-ref str i) '(#\+ #\-)))
                   (convert rad (string-ref str i) (+ i 1)))
                  (else
                   (convert rad #f i))))

          (define (radix-prefix i)
            (cond ((and (<= (+ i 3) (string-length str)) ;; need at least three chars
                        (char=? (string-ref str i) #\#)
                        (assv (char-downcase (string-ref str (+ i 1)))
                              '((#\x . 16)
                                (#\d . 10)
                                (#\o . 8)
                                (#\b . 2))))
                   =>
                   (lambda (force-radix)
                     (sign-prefix (cdr force-radix) (+ i 2))))
                  (else
                   (sign-prefix radix i))))

          (radix-prefix 0)))

      ;;---------------------------------------------------------------------------

      ;; Conversions

      (define (bignum->fixnum-list x radix-minus-1)
        (let* ((big-radix
                (bignum-add2
                 (fixnum->bignum radix-minus-1)
                 (fixnum->bignum 1)))
               (square-series
                (let loop ((square big-radix)
                           (square-list (list big-radix)))
                  (let ((new-square
                         (bignum-mul2 square square)))
                    (if (bignum< x new-square)
                        square-list
                        (loop new-square
                              (cons new-square square-list)))))))

          (define (convert n square-series tail)
            (if (pair? square-series)
                (let* ((qr (bignum-div n (car square-series)))
                       (q (car qr))
                       (r (cdr qr))
                       (new-square-series (cdr square-series)))
                  (convert r
                           new-square-series
                           (convert q
                                    new-square-series
                                    tail)))
                (let ((d (bignum->fixnum n)))
                  (if (and (null? tail) ;; avoid leading zeroes
                           (= d 0))
                      tail
                      (cons d tail)))))

          (convert x square-series '())))

      (define (fixnum-list->bignum digit-list radix-minus-1)

        ;; Note: a divide-and-conquer algorithm would be faster for large numbers.

        (let ((big-radix
               (bignum-add2
                (fixnum->bignum radix-minus-1)
                (fixnum->bignum 1))))
          (let loop ((n bignum-zero) (lst (reverse digit-list)))
            (if (pair? lst)
                (loop (bignum-add2
                       (bignum-mul2 n big-radix)
                       (fixnum->bignum (car lst)))
                      (cdr lst))
                n))))

      (define (bignum->fixnum x) ;; returns #f on fixnum overflow
        (let ((lenx-minus-1 (- (bignum-length x) 1)))
          (let loop ((n 0) (i lenx-minus-1))
            (cond ((< 0 i)
                   (if (< n (bignum-min-fixnum-div-radix))
                       #f
                       (let ((m (* n (bignum-radix)))
                             (d (bignum-digit-ref x i)))
                         (if (< m (+ (bignum-min-fixnum) d))
                             #f
                             (loop (- m d)
                                   (- i 1))))))
                  ((bignum-negative? x)
                   n)
                  ((not (= n (bignum-min-fixnum)))
                   (- 0 n))
                  (else
                   #f))))))

     (else

      ;; Implementation is much easier when the host has bignums...

      (define (bignum= x y) (= x y))
      (define (bignum< x y) (< x y))
      (define (bignum> x y) (> x y))
      (define (bignum<= x y) (<= x y))
      (define (bignum>= x y) (>= x y))
      (define (bignum-zero? x) (zero? x))
      (define (bignum-negative? x) (negative? x))
      (define (bignum-positive? x) (positive? x))
      (define (bignum-even? x) (even? x))
      (define (bignum-odd? x) (odd? x))
      (define (bignum+ . args) (apply + args))
      (define (bignum- x . args) (apply - (cons x args)))
      (define (bignum* . args) (apply * args))
      (define (bignum-quotient x y) (quotient x y))
      (define (bignum-remainder x y) (remainder x y))
      (define (bignum-modulo x y) (modulo x y))
      (define (bignum-max x . args) (apply max (cons x args)))
      (define (bignum-min x . args) (apply min (cons x args)))
      (define (bignum-abs x) (abs x))
      (define (bignum-gcd x y) (gcd x y))
      (define (bignum-lcm x y) (lcm x y))
      (define (bignum-expt x y) (expt x y))
      (define (bignum-bitwise-not x) (- -1 x))
      (define (bignum->string x . maybe-radix) ;; 2 <= radix <= 36
        (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
          (number->string x radix)))
      (define (string->bignum str . maybe-radix)
        (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
          (string->number str radix)))
      (define (bignum->fixnum x) x)
      (define (fixnum->bignum n) n)

      (define (bignum-integer-length x)

        (define (up x p list-of-two^p)
          (let ((two^p (car list-of-two^p)))
            (if (< x two^p)
                (down x p list-of-two^p 1)
                (up x
                    (* p 2)
                    (cons (* (car list-of-two^p) (car list-of-two^p))
                          list-of-two^p)))))

        (define (down x p list-of-two^p result)
          (if (= 0 p)
              result
              (let ((two^p (car list-of-two^p)))
                (if (< x two^p)
                    (down x
                          (quotient p 2)
                          (cdr list-of-two^p)
                          result)
                    (down (quotient x two^p)
                          (quotient p 2)
                          (cdr list-of-two^p)
                          (+ result p))))))

        (let ((x (if (negative? x) (bignum-bitwise-not x) x)))
          (if (= 0 x)
              0
              (up x 16 '(65536 256 16 4 2)))))

      (define (bignum->fixnum-list x radix-minus-1)
        (let* ((big-radix
                (+ radix-minus-1 1))
               (square-series
                (let loop ((square big-radix)
                           (square-list (list big-radix)))
                  (let ((new-square
                         (* square square)))
                    (if (< x new-square)
                        square-list
                        (loop new-square
                              (cons new-square square-list)))))))

          (define (convert n square-series tail)
            (if (pair? square-series)
                (let* ((q (quotient n (car square-series)))
                       (r (remainder n (car square-series)))
                       (new-square-series (cdr square-series)))
                  (convert r
                           new-square-series
                           (convert q
                                    new-square-series
                                    tail)))
                (let ((d n))
                  (if (and (null? tail) ;; avoid leading zeroes
                           (= d 0))
                      tail
                      (cons d tail)))))

          (convert x square-series '())))

      (define (fixnum-list->bignum digit-list radix-minus-1)

        ;; Note: a divide-and-conquer algorithm would be faster for large numbers.

        (let ((big-radix (+ radix-minus-1 1)))
          (let loop ((n 0) (lst (reverse digit-list)))
            (if (pair? lst)
                (loop (+ (* n big-radix) (car lst))
                      (cdr lst))
                n))))))

;;;---------------------------------------------------------------------------

    (define (bignum->u8vector n)
      (u8-list->bytevector (reverse (bignum->fixnum-list n 255))))

    (define (u8vector->bignum u8vect)
      (fixnum-list->bignum (reverse (bytevector->u8-list u8vect)) 255))

;    (define (bignum->base64-string n)
;      (u8vector->base64-string (bignum->u8vector n)))

;    (define (base64-string->bignum str)
;      (u8vector->bignum (base64-string->u8vector str)))

;;;---------------------------------------------------------------------------

    (define (bignum-expt-mod x y m)

      ;; computes x^y mod m

      ;; TODO: use Montgomery algorithm

      (define (expt-mod n e m)
        (cond ((bignum-zero? e)
               (fixnum->bignum 1))
              ((bignum-even? e)
               (expt-mod
                (bignum-modulo (bignum* n n) m)
                (bignum-quotient e (fixnum->bignum 2))
                m))
              (else
               (bignum-modulo
                (bignum*
                 n
                 (expt-mod n (bignum- e (fixnum->bignum 1)) m))
                m))))

      (expt-mod x y m))

;;;============================================================================


    ))
