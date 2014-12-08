;; -*- scheme -*-
;; srfi-27, Sources of Random Bits
;; http://srfi.schemers.org/srfi-27/srfi-27.html
;; http://wiki.call-cc.org/eggref/4/srfi-27
;; http://practical-scheme.net/gauche/man/gauche-refe_118.html#Sources-of-random-bits

(define-library (srfi 27)
  (export
   random-integer
   random-real
   default-random-source
   make-random-source
   random-source?
   random-source-state-ref
   random-source-state-set!
   random-source-randomize!
   random-source-pseudo-randomize!
   random-source-make-integers
   random-source-make-reals)

  (import (scheme base)
          (scheme time)
          (scheme write) ;; XXX
          )
  (cond-expand
   ((or foment kawa)
    (include "srfi-27/srfi-27-a.scm"
             "srfi-27/mrg32k3a-a.scm"
             "srfi-27/mrg32k3a.scm"))
   ;; (gauche (import (srfi-27)))
   ;; (sagittarius (import (srfi :27)))
   ))
