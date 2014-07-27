#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
exec gosh \
-e '(append! *load-suffixes* (list ".sld"))' \
-e "(append! *load-path* (list \"$DIR\"))" \
-r7 "$0" "$@"
|#

(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
