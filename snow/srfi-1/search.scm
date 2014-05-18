;; search.scm -- list searching and splitting
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt


(define (%cars+cdrs lists)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
           (let-values (((list other-lists) (car+cdr lists)))
             (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
                 (let-values (((a d) (car+cdr list)))
                   (let-values (((cars cdrs) (recur other-lists)))
                     (values (cons a cars) (cons d cdrs))))))
           (values '() '()))))))

(define (find pred list)
  (cond ((find-tail pred list) => car)
        (else #f)))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
         (if (pred (car list)) list
             (lp (cdr list))))))

(define (take-while pred ls)
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse! res))))

(define take-while! take-while)

(define (drop-while pred ls)
  (or (find-tail (lambda (x) (not (pred x))) ls) '()))

(define (span pred ls)
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (values (reverse! res) ls))))

(define span! span)

(define (break pred ls) (span (lambda (x) (not (pred x))) ls))

(define break! break)

(define (any pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let-values (((heads tails) (%cars+cdrs (cons lis1 lists))))
        (and (pair? heads)
             (let lp ((heads heads) (tails tails))
               (let-values (((next-heads next-tails) (%cars+cdrs tails)))
                 (if (pair? next-heads)
                     (or (apply pred heads)
                         (lp next-heads next-tails))
                     (apply pred heads)))))) ; Last PRED app is tail call.
      ;; Fast path
      (and (not (null-list? lis1))
           (let lp ((head (car lis1)) (tail (cdr lis1)))
             (if (null-list? tail)
                 (pred head) ; Last PRED app is tail call.
                 (or (pred head) (lp (car tail) (cdr tail))))))))


(define (every pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let-values (((heads tails) (%cars+cdrs (cons lis1 lists))))
        (or (not (pair? heads))
            (let lp ((heads heads) (tails tails))
              (let-values (((next-heads next-tails) (%cars+cdrs tails)))
                (if (pair? next-heads)
                    (and (apply pred heads)
                         (lp next-heads next-tails))
                    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-list? lis1)
          (let lp ((head (car lis1))  (tail (cdr lis1)))
            (if (null-list? tail)
                (pred head) ; Last PRED app is tail call.
                (and (pred head) (lp (car tail) (cdr tail))))))))


(define (list-index pred ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (n 0))
        (and (pair? ls) (if (pred (car ls)) n (lp (cdr ls) (+ n 1)))))
      (let lp ((lists (cons ls lists)) (n 0))
        (and (every pair? lists)
             (if (apply pred (map car lists)) n (lp (map cdr lists) (+ n 1)))
             ))))
