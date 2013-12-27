#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)
;; (import-for-syntax r7rs)
(use inclub)
(use http-client)
(use srfi-69)

(require-library scheme.process-context)
(import (scheme process-context))

;; (inclub "seth/tar.sld")
(include "seth/tar.sld")
(include "seth/snow2-utils.sld")
(import (chicken) (prefix (seth tar) tar-) (seth snow2-utils))

(inclub "snow2-client-common.scm")


;; (define (report-error format-string . args)
;;   (apply format (cons (current-error-port) (cons format-string args)))
;;   #f)


(main-program)
