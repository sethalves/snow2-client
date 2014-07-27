#! /bin/bash
#| -*- scheme -*-
exec foment -A /usr/local/share/scheme "$0" "$@"
|#

;; FOMENT_LIBPATH

(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
