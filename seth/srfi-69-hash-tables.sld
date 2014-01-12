;; http://srfi.schemers.org/srfi-69/srfi-69.html

(define-library (seth srfi-69-hash-tables)
  (export make-hash-table
          hash-table?
          hash-table-copy
          hash-table-delete!
          hash-table-merge!
          hash-table-exists?
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-update!
          hash-table-update!/default
          hash-table-keys
          hash-table-values
          hash-table->alist
          alist->hash-table
          )
  (import (scheme base))
  (cond-expand
   (chibi (import (srfi 69)))
   (chicken (import (srfi 69)))
   (gauche (import (gauche dictionary) (util trie)))
   (sagittarius (import (rnrs))))
  (begin
    (cond-expand

     (chicken)

     (chibi)

     (gauche
      ;; http://practical-scheme.net/gauche/man/gauche-refe_53.html
      ;; (define make-hash-table-real
      ;;   (if (global-variable-bound? (current-module) 'make-hash-table-real)
      ;;       make-hash-table-real
      ;;       make-hash-table))
      ;; (define (make-hash-table . args)
      ;;   (make-hash-table-real 'equal?))
      (define hash-table-ref hash-table-get)
      (define hash-table-ref/default hash-table-get)
      (define hash-table-set! hash-table-put!))

     (sagittarius
      ;; http://ktakashi.github.io/sagittarius-ref.html#rnrs.hashtables.6
      (define (make-hash-table . args)
        (let* ((args-len (length args))
               (tester (if (> args-len 0) (list-ref args 0) equal?))
               (hasher (if (> args-len 1) (list-ref args 1) equal-hash)))
          (make-hashtable hasher tester)))
      (define hash-table? hashtable?)
      (define (hash-table-copy hash-table)
        (hashtable-copy hash-table #t))
      (define hash-table-ref hashtable-ref)
      (define hash-table-ref/default hashtable-ref)
      (define hash-table-set! hashtable-set!)
      (define (hash-table-keys hash-table)
        (vector->list (hashtable-keys hash-table)))
      (define (hash-table-values hash-table)
        (vector->list (hashtable-entries hash-table)))
      (define hash-table-exists? hashtable-contains?)
      (define hash-table-delete! hashtable-delete!)
      (define (hash-table-update! hash-table key function)
        (hash-table-set! hash-table key
                         (function (hash-table-ref hash-table key))))
      (define hash-table-update!/default hashtable-update!)))


    (cond-expand
     ((or gauche)
      (define (hash-table-update!/default table key func default)
        (hash-table-set!
         table key
         (func (hash-table-ref/default table key default)))))
     (else))


     (cond-expand
      ((or sagittarius)
       (define (hash-table->alist table)
         (map
          (lambda (key) (cons key (hash-table-ref table key)))
          (hash-table-keys table))))
      (else))


     (cond-expand
      ((or sagittarius)
       (define (alist->hash-table alist)
         (let ((table (make-hash-table)))
           (for-each
            (lambda (nv)
              (hash-table-set! table (car nv) (cdr nv)))
            alist)
           table)))
      (else))


     (cond-expand
      ((or sagittarius)
       (define (hash-table-merge! dst-table src-table)
         (for-each
          (lambda (key)
            (hash-table-set! dst-table key (hash-table-ref src-table)))
          (hash-table-keys src-table))))
      (else))

     ))
