snow2-client
============

Client for snow2 repositories.

This is alpha-quality software.  It might delete your files!

snow2-client is experimental software for finding and installing
scheme libraries which are portable between r7rs schemes.  The
libraries are stored in repositories which are described in:

  http://trac.sacrideo.us/wg/wiki/Snow

Inspiration and some packages are taken from the first snow system:

  http://snow.iro.umontreal.ca/

Supported schemes are:

<a href="http://code.google.com/p/chibi-scheme/">Chibi</a>,
<a href="http://call-cc.org/">Chicken</a>,
<a href="https://code.google.com/p/foment/">Foment</a>,
<a href="http://practical-scheme.net/gauche/">Gauche</a>,
<a href="https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home">Sagittarius</a>



Other schemes will be supported when they support r7rs-style libraries.

Requirements
============

Last updated: 2014-07-20

### Chibi

Snow2 works with Chibi-Scheme 0.7.  Chibi also comes with its own client
called snow-chibi.

### CHICKEN

CHICKEN 4.9.0.1 or newer is required to run snow2.

Running under CHICKEN requires that several eggs be installed:

```
chicken-install srfi-27 srfi-29 srfi-37 http-client openssl udp r7rs
```

Additionally, some packages assume you have certain chicken eggs installed.

### Foment

Build Foment from git.

### Gauche

Gauche 0.9.4 can run snow2.

### Sagittarius

Snow2 works with sagittarius-0.5.3.


Installation
============

```
make SCHEME=scheme install
```

*scheme* can be any of chibi, chicken, foment, gauche, sagittarius.  For example:

```
make SCHEME=chicken install
```

snow2 will be placed in /usr/local/bin/

Some libraries may be placed in /usr/local/share/

You can also use "build" rather than "install".  This does nothing unless
SCHEME=chicken, in which case it compiles the client.


Running
=======

```
$ snow2 -h
snow2 [arguments] <operation> '(library name)' ...
  <operation> can be one of: install uninstall list-depends search check
  -r --repo <url>      Add to list of snow2 repositories.
  -s --symlink         Make symlinks to a repo's source files.
  -v --verbose         Print more.
  -h --help            Print usage message.

Example: snow2 install '(snow hello)'
```

### operations

#### install

The requested libraries will be made available as children of the
current directory.  If the package holding the requested libraries is
located on an HTTP server, it will be downloaded to a file in /tmp/
and untarred (and the tarball in /tmp will be removed).  If the
repository is a directory on the local file system, the package will
be untarred from the tarball located in the repository's directory.
If the repository is a local directory and the --symlink option is
used, the installed libraries will be symbolic links to the source
files in the local repository.  This can be useful when working on
changes to a library.

If no repository urls (or filesystem paths) are indicated on
the command-line, the default repository will be used.

  http://snow2.s3-website-us-east-1.amazonaws.com/

#### uninstall

This currently does nothing.

#### list-depends

The arguments of the (depends ...)  clause of each package containing
the mentioned libraries will be printed.

#### search

Display a list of libraries which have names that are matched by
a substring search with the provided argument.

#### check

This is a lint-like function for use on repository sources.

### examples

```
$ snow2 search hello
(snow hello)
```

```
$ snow2 search 'ow he'
(snow hello)
```


```
$ snow2 list-depends '(snow hello)'
(snow pi)
(snow hello)
(snow bignum)
(snow bytevector)
```

```
$ snow2 install '(snow hello)'
downloading hello.tgz from http://snow-repository.s3-website-us-east-1.amazonaws.com/
downloading bignum.tgz from http://snow-repository.s3-website-us-east-1.amazonaws.com/
downloading bytevector.tgz from http://snow-repository.s3-website-us-east-1.amazonaws.com/
downloading pi.tgz from http://snow-repository.s3-website-us-east-1.amazonaws.com/
$ ls snow
bignum.sld  bytevector.sld  hello.sld  pi.sld
```

```
$ snow2 search
(snow assert)
(snow bignum)
(snow binio)
...

```

See <a href="https://github.com/sethalves/snow2-test-chicken">snow2-test-chicken</a> or <a href="https://github.com/sethalves/snow2-test-chibi">snow2-test-chibi</a> for more examples.

Other Scheme Package Managers
=============================

http://gna.org/projects/spells

http://home.gna.org/dorodango/manual/

http://planet.racket-lang.org/

http://synthcode.com/scheme/common-scheme/doc/common-scheme.html

http://wiki.call-cc.org/eggs

http://www.gnu.org/software/guix/

http://www.schemespheres.org/
