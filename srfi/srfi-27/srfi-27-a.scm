; MODULE DEFINITION FOR SRFI-27
; =============================
; 
; Sebastian.Egner@philips.com, Mar-2002, in Scheme 48 0.57
;
; This file contains the top-level definition for the 54-bit integer-only
; implementation of SRFI-27 for the Scheme 48 0.57 system. 
;
; 1. The core generator is implemented in 'mrg32k3a-a.scm'.
; 2. The generic parts of the interface are in 'mrg32k3a.scm'.
; 3. The non-generic parts (record type, time, error) are here.
;
; creating the module:
;   ,config ,load srfi-27-a.scm
;
; loading the module, once created:
;   ,open srfi-27
;
; history of this file:
;   SE, 22-Mar-2002: initial version
;   SE, 27-Mar-2002: checked again


;; (open
;;  scheme-level-1
;;  (subset srfi-9 (define-record-type)) 
;;  (subset srfi-23 (error))
;;  (subset posix-time (current-time))
;;  (subset posix (time-seconds)))

(define-record-type :random-source
  (:random-source-make
   state-ref
   state-set!
   randomize!
   pseudo-randomize!
   make-integers
   make-reals)
  :random-source?
  (state-ref :random-source-state-ref)
  (state-set! :random-source-state-set!)
  (randomize! :random-source-randomize!)
  (pseudo-randomize! :random-source-pseudo-randomize!)
  (make-integers :random-source-make-integers)
  (make-reals :random-source-make-reals))

(define (:random-source-current-time)
  (exact (round (current-second))))
