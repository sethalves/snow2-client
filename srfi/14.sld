(define-library (srfi 14)
  (export char-set? char-set= char-set<=
          char-set-hash
          char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
          char-set-fold char-set-unfold char-set-unfold!
          char-set-for-each char-set-map
          char-set-copy char-set

          list->char-set string->char-set
          list->char-set! string->char-set!

          char-set-filter ucs-range->char-set ->char-set
          char-set-filter! ucs-range->char-set!

          char-set->list char-set->string

          char-set-size char-set-count char-set-contains?
          char-set-every char-set-any

          char-set-adjoin char-set-delete
          char-set-adjoin! char-set-delete!

          char-set-complement char-set-union char-set-intersection
          char-set-complement! char-set-union! char-set-intersection!

          char-set-difference char-set-xor char-set-diff+intersection
          char-set-difference! char-set-xor! char-set-diff+intersection!

          char-set:lower-case char-set:upper-case char-set:title-case
          char-set:letter char-set:digit char-set:letter+digit
          char-set:graphic char-set:printing char-set:whitespace
          char-set:iso-control char-set:punctuation char-set:symbol
          char-set:hex-digit char-set:blank char-set:ascii
          char-set:empty char-set:full)

  (import (scheme base))
  (cond-expand
   ;; ((or chicken gauche sagittarius)
   ;;  (import (srfi 14)))
   (sagittarius (import (srfi :14)))
   (chibi
    (import (only (srfi 1) fold filter)
            (chibi char-set)
            (chibi char-set full)
            (rename (chibi char-set extras)
                    (list->char-set chibi-list->char-set)
                    (ucs-range->char-set chibi-ucs-range->char-set)
                    (char-set-adjoin chibi-char-set-adjoin)
                    (char-set-adjoin! chibi-char-set-adjoin!)
                    )
            ;; (srfi 33)
            (chibi iset)
            (chibi iset iterators)
            ))
   (else
    (import (only (srfi 60) bitwise-and))))

  (cond-expand
   ((or chicken gauche sagittarius))
   (chibi
    (begin

      (define char-set= iset=)
      (define char-set<= iset<=)
      (define char-set-cursor iset-cursor)
      (define (char-set-ref cset cursor)
        (integer->char (iset-ref cset cursor)))
      (define char-set-cursor-next iset-cursor-next)
      (define end-of-char-set? end-of-iset?)
      (define char-set-fold iset-fold)
      (define (char-set-for-each proc cs)
        (iset-for-each (lambda (c) (proc (integer->char c))) cs))
      (define (char-set-map proc cset)
        (iset-map (lambda (c) (char->integer (proc (integer->char c)))) cset))
      (define char-set-size iset-size)
      (define (char-set-count pred cs)
        (char-set-size (char-set-filter pred cs)))
      (define string->char-set! string->char-set!)
      (define (char-set-delete cs . chars)
        (let loop ((cs cs)
                   (chars chars))
          (cond ((null? chars) cs)
                (else
                 (loop (iset-delete cs (char->integer (car chars)))
                       (cdr chars))))))
      (define char-set-delete! char-set-delete)
      (define (char-set-hash cs . maybe-bound)
        ;; XXX this is terrible
        (modulo (fold + 0 (iset->list cs))
                (if (pair? maybe-bound) (car maybe-bound) 4194304)))

      (define (char-set-unfold p f g seed . maybe-base-cs)
        (let ((base-cs (if (pair? maybe-base-cs)
                           (car maybe-base-cs)
                           (iset))))
          (char-set-unfold! p f g seed (char-set-copy base-cs))))

      (define (char-set-unfold! p f g seed . maybe-base-cs)
        (let ((base-cs (if (pair? maybe-base-cs)
                           (car maybe-base-cs)
                           (iset))))
          (let lp ((seed seed) (cs base-cs))
            (if (p seed) cs ; P says we are done.
                (lp (g seed) ; Loop on (G SEED).
                    (char-set-adjoin! cs (f seed))))))) ; Add (F SEED) to set.

      (define (list->char-set ls . maybe-base-cs)
        (let ((base-cs (if (pair? maybe-base-cs)
                           (car maybe-base-cs)
                           (iset))))
          (char-set-union
           (chibi-list->char-set ls)
           base-cs)))

      (define list->char-set! list->char-set)

      (define (char-set-filter predicate domain . maybe-base-cs)
        (let ((base-cs (if (pair? maybe-base-cs)
                           (car maybe-base-cs)
                           (iset))))
          (char-set-union
           (list->char-set (filter predicate (char-set->list domain)))
           base-cs)))

      (define char-set-filter! char-set-filter)

      (define (ucs-range->char-set start end . rest)
        (let* ((rest-len (length rest))
               (error? (if (> rest-len 0) (list-ref rest 0) #f))
               (bs (if (> rest-len 1) (list-ref rest 1) (iset))))
          (char-set-union
           (chibi-ucs-range->char-set start end)
           bs)))

      (define ucs-range->char-set! ucs-range->char-set)

      (define (->char-set x)
        (cond ((char-set? x) x)
              ((string? x) (string->char-set x))
              ((char? x) (char-set x))
              (else (error "->char-set can't convert to char-set" x))))


      (define (char-set-every pred cs)
        (let loop ((chars (char-set->list cs)))
          (cond ((null? chars) #t)
                ((not (pred (car chars))) #f)
                (else (loop (cdr chars))))))

      (define (char-set-any pred cs)
        (let loop ((chars (char-set->list cs)))
          (cond ((null? chars) #f)
                ((pred (car chars)) => (lambda (x) x))
                (else (loop (cdr chars))))))


      (define (char-set-adjoin cs . chars)
        (let loop ((cs cs)
                   (chars chars))
          (cond ((null? chars) cs)
                (else
                 (loop (chibi-char-set-adjoin cs (car chars))
                       (cdr chars))))))

      (define (char-set-adjoin! cs . chars)
        (let loop ((cs cs)
                   (chars chars))
          (cond ((null? chars) cs)
                (else
                 (loop (chibi-char-set-adjoin! cs (car chars))
                       (cdr chars))))))

      (define char-set-complement! char-set-complement)

      (define (char-set-xor! a . args)
        (let ((b (and (pair? args) (car args))))
          (cond
           (b
            (iset-for-each
             (lambda (i)
               (let ((p (iset-contains? a i))
                     (q (iset-contains? b i)))
                 (if (and (or p q) (not (and p q)))
                     (if (not p) (iset-adjoin! a i))
                     (if p (iset-delete! a i)))))
             (iset-union a b))
            (apply char-set-xor! a (cdr args)))
           (else a))))

      (define (char-set-xor cs . args)
        (apply char-set-xor! (cons (char-set-copy cs) args)))

      (define (char-set-diff+intersection cs1 cs2 . args)
        (values
         (apply char-set-difference (cons cs1 (cons cs2 args)))
         (char-set-intersection cs1 (apply char-set-union (cons cs2 args)))))

      (define char-set-diff+intersection! char-set-diff+intersection)

      ))
   (else
    (include "srfi-14.scm"))))
