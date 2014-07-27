#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
exec sash -A "$DIR" -S .sld "$0" "$@"
|#

;; #| -*- scheme -*-
;; exec sash -A /usr/local/share/scheme -A . -S .sld "$0" "$@"
;; |#


(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
