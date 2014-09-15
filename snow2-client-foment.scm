#! /bin/bash
#| -*- scheme -*-
exec foment -A . "$0" "$@"
|#


(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
