;; http://srfi.schemers.org/srfi-69/srfi-69.html

(define-library (srfi 69)
  (export make-hash-table
          hash-table?
          hash-table-copy
          hash-table-delete!
          hash-table-merge!
          hash-table-exists?
          hash-table-walk
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-update!
          hash-table-update!/default
          hash-table-keys
          hash-table-values
          hash-table->alist
          alist->hash-table
          hash-table-cons!
          hash-table-size
          )
  (import (scheme base))
  (cond-expand
   (foment (import (scheme char)
                   (scheme cxr)
                   (scheme complex)))
   (gauche
    (import 
     (prefix (only (gauche base)
                   make-hash-table
                   hash-table?
                   hash-table-get
                   hash-table-exists?
                   hash-table-put!
                   hash-table-copy
                   hash-table-delete!
                   hash-table-update!
                   hash-table-keys
                   hash-table-values
                   hash-table->alist
                   alist->hash-table) gauche-)))
   (sagittarius (import (rnrs)))
   )
  (begin
    (cond-expand
     (gauche
      ;; http://practical-scheme.net/gauche/man/gauche-refe_55.html
      (define (make-hash-table . args)
        (gauche-make-hash-table 'equal?))
      (define hash-table? gauche-hash-table?)
      (define hash-table-ref gauche-hash-table-get)
      (define hash-table-ref/default gauche-hash-table-get)
      (define hash-table-exists? gauche-hash-table-exists?)
      (define hash-table-set! gauche-hash-table-put!)
      (define hash-table-copy gauche-hash-table-copy)
      (define hash-table-delete! gauche-hash-table-delete!)
      (define hash-table-update! gauche-hash-table-update!)
      (define hash-table-keys gauche-hash-table-keys)
      (define hash-table-values gauche-hash-table-values)
      (define hash-table->alist gauche-hash-table->alist)
      (define alist->hash-table gauche-alist->hash-table)
      )

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
        (map (lambda (key)
               (hash-table-ref hash-table key))
             (hash-table-keys hash-table)))
      (define hash-table-exists? hashtable-contains?)
      (define hash-table-delete! hashtable-delete!)
      (define (hash-table-update! hash-table key function)
        (hash-table-set! hash-table key
                         (function (hash-table-ref hash-table key))))
      (define hash-table-update!/default hashtable-update!))


     (else
      ;; srfi-69 reference implementation
      (define *default-bound* (- (expt 2 29) 3))

      (define (%string-hash s ch-conv bound)
        (let ((hash 31)
              (len (string-length s)))
          (do ((index 0 (+ index 1)))
              ((>= index len) (modulo hash bound))
            (set! hash (modulo
                        (+ (* 37 hash)
                           (char->integer (ch-conv (string-ref s index))))
                        *default-bound*)))))

      (define (string-hash s . maybe-bound)
        (let ((bound (if (null? maybe-bound)
                         *default-bound*
                         (car maybe-bound))))
          (%string-hash s (lambda (x) x) bound)))

      (define (string-ci-hash s . maybe-bound)
        (let ((bound (if (null? maybe-bound)
                         *default-bound*
                         (car maybe-bound))))
          (%string-hash s char-downcase bound)))

      (define (symbol-hash s . maybe-bound)
        (let ((bound (if (null? maybe-bound)
                         *default-bound*
                         (car maybe-bound))))
          (%string-hash (symbol->string s) (lambda (x) x) bound)))

      (define (hash obj . maybe-bound)
        (let ((bound (if (null? maybe-bound)
                         *default-bound*
                         (car maybe-bound))))
          (cond ((integer? obj) (modulo obj bound))
                ((string? obj) (string-hash obj bound))
                ((symbol? obj) (symbol-hash obj bound))
                ((real? obj)
                 (modulo (+ (numerator obj) (denominator obj)) bound))
                ((number? obj)
                 (modulo (+ (hash (real-part obj))
                            (* 3 (hash (imag-part obj))))
                         bound))
                ((char? obj) (modulo (char->integer obj) bound))
                ((vector? obj) (vector-hash obj bound))
                ((pair? obj) (modulo (+ (hash (car obj))
                                        (* 3 (hash (cdr obj))))
                                     bound))
                ((null? obj) 0)
                ((not obj) 0)
                ((procedure? obj)
                 (error "hash: procedures cannot be hashed" obj))
                (else 1))))

      (define hash-by-identity hash)

      (define (vector-hash v bound)
        (let ((hashvalue 571)
              (len (vector-length v)))
          (do ((index 0 (+ index 1)))
              ((>= index len) (modulo hashvalue bound))
            (set! hashvalue (modulo (+ (* 257 hashvalue)
                                       (hash (vector-ref v index)))
                                    *default-bound*)))))

      (define %make-hash-node cons)
      (define %hash-node-set-value! set-cdr!)
      (define %hash-node-key car)
      (define %hash-node-value cdr)

      (define-record-type <srfi-hash-table>
        (%make-hash-table size hash compare associate entries)
        hash-table?
        (size hash-table-size hash-table-set-size!)
        (hash hash-table-hash-function)
        (compare hash-table-equivalence-function)
        (associate hash-table-association-function)
        (entries hash-table-entries hash-table-set-entries!))

      (define *default-table-size* 64)

      (define (appropriate-hash-function-for comparison)
        (or (and (eq? comparison eq?) hash-by-identity)
            (and (eq? comparison string=?) string-hash)
            (and (eq? comparison string-ci=?) string-ci-hash)
            hash))

      (define (make-hash-table . args)
        (let* ((comparison (if (null? args) equal? (car args)))
               (hash
                (if (or (null? args) (null? (cdr args)))
                    (appropriate-hash-function-for comparison) (cadr args)))
               (size
                (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                    *default-table-size* (caddr args)))
               (association
                (or (and (eq? comparison eq?) assq)
                    (and (eq? comparison eqv?) assv)
                    (and (eq? comparison equal?) assoc)
                    (letrec
                        ((associate
                          (lambda (val alist)
                            (cond ((null? alist) #f)
                                  ((comparison val (caar alist)) (car alist))
                                  (else (associate val (cdr alist)))))))
                      associate))))
          (%make-hash-table 0 hash comparison association
                            (make-vector size '()))))

      (define (make-hash-table-maker comp hash)
        (lambda args (apply make-hash-table (cons comp (cons hash args)))))
      (define make-symbol-hash-table
        (make-hash-table-maker eq? symbol-hash))
      (define make-string-hash-table
        (make-hash-table-maker string=? string-hash))
      (define make-string-ci-hash-table
        (make-hash-table-maker string-ci=? string-ci-hash))
      (define make-integer-hash-table
        (make-hash-table-maker = modulo))

      (define (%hash-table-hash hash-table key)
        ((hash-table-hash-function hash-table)
         key (vector-length (hash-table-entries hash-table))))

      (define (%hash-table-find entries associate hash key)
        (associate key (vector-ref entries hash)))

      (define (%hash-table-add! entries hash key value)
        (vector-set! entries hash
                     (cons (%make-hash-node key value)
                           (vector-ref entries hash))))

      (define (%hash-table-delete! entries compare hash key)
        (let ((entrylist (vector-ref entries hash)))
          (cond ((null? entrylist) #f)
                ((compare key (caar entrylist))
                 (vector-set! entries hash (cdr entrylist)) #t)
                (else
                 (let loop ((current (cdr entrylist)) (previous entrylist))
                   (cond ((null? current) #f)
                         ((compare key (caar current))
                          (set-cdr! previous (cdr current)) #t)
                         (else (loop (cdr current) current))))))))

      (define (%hash-table-walk proc entries)
        (do ((index (- (vector-length entries) 1) (- index 1)))
            ((< index 0)) (for-each proc (vector-ref entries index))))

      (define (%hash-table-maybe-resize! hash-table)
        (let* ((old-entries (hash-table-entries hash-table))
               (hash-length (vector-length old-entries)))
          (if (> (hash-table-size hash-table) hash-length)
              (let* ((new-length (* 2 hash-length))
                     (new-entries (make-vector new-length '()))
                     (hash (hash-table-hash-function hash-table)))
                (%hash-table-walk
                 (lambda (node)
                   (%hash-table-add!
                    new-entries
                    (hash (%hash-node-key node) new-length)
                    (%hash-node-key node) (%hash-node-value node)))
                 old-entries)
                (hash-table-set-entries! hash-table new-entries)))))

      (define (hash-table-ref hash-table key . maybe-default)
        (cond ((%hash-table-find (hash-table-entries hash-table)
                                 (hash-table-association-function hash-table)
                                 (%hash-table-hash hash-table key) key)
               => %hash-node-value)
              ((null? maybe-default)
               (error "hash-table-ref: no value associated with" key))
              (else ((car maybe-default)))))

      (define (hash-table-ref/default hash-table key default)
        (hash-table-ref hash-table key (lambda () default)))

      (define (hash-table-set! hash-table key value)
        (let ((hash (%hash-table-hash hash-table key))
              (entries (hash-table-entries hash-table)))
          (cond ((%hash-table-find entries
                                   (hash-table-association-function hash-table)
                                   hash key)
                 => (lambda (node) (%hash-node-set-value! node value)))
                (else (%hash-table-add! entries hash key value)
                      (hash-table-set-size! hash-table
                                            (+ 1 (hash-table-size hash-table)))
                      (%hash-table-maybe-resize! hash-table)))))

      (define (hash-table-update! hash-table key function . maybe-default)
        (let ((hash (%hash-table-hash hash-table key))
              (entries (hash-table-entries hash-table)))
          (cond ((%hash-table-find entries
                                   (hash-table-association-function hash-table)
                                   hash key)
                 => (lambda (node)
                      (%hash-node-set-value!
                       node (function (%hash-node-value node)))))
                ((null? maybe-default)
                 (error "hash-table-update!: no value exists for key" key))
                (else (%hash-table-add! entries hash key
                                        (function ((car maybe-default))))
                      (hash-table-set-size! hash-table
                                            (+ 1 (hash-table-size hash-table)))
                      (%hash-table-maybe-resize! hash-table)))))

      (define (hash-table-update!/default hash-table key function default)
        (hash-table-update! hash-table key function (lambda () default)))

      (define (hash-table-delete! hash-table key)
        (if (%hash-table-delete! (hash-table-entries hash-table)
                                 (hash-table-equivalence-function hash-table)
                                 (%hash-table-hash hash-table key) key)
            (hash-table-set-size! hash-table
                                  (- (hash-table-size hash-table) 1))))

      (define (hash-table-exists? hash-table key)
        (and (%hash-table-find (hash-table-entries hash-table)
                               (hash-table-association-function hash-table)
                               (%hash-table-hash hash-table key) key) #t))

      (define (hash-table-walk hash-table proc)
        (%hash-table-walk
         (lambda (node) (proc (%hash-node-key node) (%hash-node-value node)))
         (hash-table-entries hash-table)))

      (define (hash-table-fold hash-table f acc)
        (hash-table-walk hash-table
                         (lambda (key value) (set! acc (f key value acc))))
        acc)

      (define (alist->hash-table alist . args)
        (let* ((comparison (if (null? args) equal? (car args)))
               (hash
                (if (or (null? args) (null? (cdr args)))
                    (appropriate-hash-function-for comparison) (cadr args)))
               (size
                (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                    (max *default-table-size*
                         (* 2 (length alist))) (caddr args)))
               (hash-table (make-hash-table comparison hash size)))
          (for-each
           (lambda (elem)
             (hash-table-update!/default
              hash-table (car elem) (lambda (x) x) (cdr elem)))
           alist)
          hash-table))

      (define (hash-table->alist hash-table)
        (hash-table-fold hash-table
                         (lambda (key val acc) (cons (cons key val) acc)) '()))

      (define (hash-table-copy hash-table)
        (let ((new (make-hash-table (hash-table-equivalence-function hash-table)
                                    (hash-table-hash-function hash-table)
                                    (max *default-table-size*
                                         (* 2 (hash-table-size hash-table))))))
          (hash-table-walk hash-table
                           (lambda (key value) (hash-table-set! new key value)))
          new))

      (define (hash-table-merge! hash-table1 hash-table2)
        (hash-table-walk
         hash-table2
         (lambda (key value) (hash-table-set! hash-table1 key value)))
        hash-table1)

      (define (hash-table-keys hash-table)
        (hash-table-fold hash-table (lambda (key val acc) (cons key acc)) '()))

      (define (hash-table-values hash-table)
        (hash-table-fold hash-table (lambda (key val acc) (cons val acc)) '()))

      ))


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
      ((or sagittarius gauche)
       (define (hash-table-merge! dst-table src-table)
         (for-each
          (lambda (key)
            (hash-table-set! dst-table key (hash-table-ref src-table)))
          (hash-table-keys src-table))))
      (else))

     (define (hash-table-cons! ht key value)
       (cond ((hash-table-exists? ht key)
              (let ((previous (hash-table-ref ht key)))
                (hash-table-set! ht key (cons value previous))))
             (else
              (hash-table-set! ht key (list value)))))
     ))
