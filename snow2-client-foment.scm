#! /bin/sh
#| -*- scheme -*-
exec foment -A /usr/local/share/scheme -A . $0 "$@"
|#

(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
