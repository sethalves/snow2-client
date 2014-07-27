#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
# Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
X=$CHIBI_MODULE_PATH
CHIBI_MODULE_PATH="" exec chibi-scheme -A "$DIR" -A "$X" -s "$0" "$@"
|#

;; #| -*- scheme -*-
;; X=$CHIBI_MODULE_PATH
;; CHIBI_MODULE_PATH="" exec chibi-scheme -A /usr/local/share/scheme -A . -A $X -s "$0" "$@"
;; |#

(import (scheme base)
        (prefix (seth snow2 client) snow2-))
(snow2-main-program)
