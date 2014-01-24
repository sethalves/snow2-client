;; -*- scheme -*-
;; srfi-27, Sources of Random Bits
;; http://srfi.schemers.org/srfi-27/srfi-27.html
;; http://wiki.call-cc.org/eggref/4/srfi-27
;; http://practical-scheme.net/gauche/man/gauche-refe_118.html#Sources-of-random-bits

(define-library (seth srfi-27-random)
  (export random-integer random-source-randomize! default-random-source)
  (import (scheme base))
  (cond-expand
   (chibi (import (srfi 27)))
   (chicken (import (srfi 27)))
   (gauche (import (gauche) (srfi-27)))
   (sagittarius (import (srfi :27))))
  (begin

    (cond-expand

     ((or chibi chicken sagittarius))

     (gauche
      (define random-integer random-integer)
      (define random-source-randomize! random-source-randomize!)
      (define default-random-source default-random-source)
      ))

    (random-source-randomize! default-random-source)
    ))
