snow2-client
============

Client for snow2 repositories.

This is alpha-quality software.  It might delete your files!

snow2-client is experimental software for finding and installing
portable scheme libraries.  The libraries are stored in repositories
which are described in:

  http://trac.sacrideo.us/wg/wiki/Snow

Inspiration and some packages are taken from the first snow system:

  http://snow.iro.umontreal.ca/

Supported schemes are:

<a href="http://synthcode.com/scheme/chibi/">Chibi</a>,
<a href="http://call-cc.org/">Chicken</a>,
<a href="http://practical-scheme.net/gauche/">Gauche</a>,
<a href="https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home">Sagittarius</a>

Other schemes will be supported when they support r7rs-style libraries.

Requirements
============

Snow2 currently (2014-1-26) requires built-from-trunk versions of all 4
supported schemes.

Chicken requires that several eggs be installed, one from svn.

```
chicken-install srfi-27 srfi-37 http-client
svn co https://code.call-cc.org/svn/chicken-eggs/release/4/r7rs/trunk r7rs
cd r7rs
chicken-install
```

Additionally, some of the snow2 packages assume you have certain chicken eggs installed.


Building
========

```
make SCHEME=scheme install
```

*scheme* can be any of chibi, chicken, gauche, sagittarius.  For example:

```
make SCHEME=chicken install
```

snow2 will be placed in /usr/local/bin/

Some libraries will be placed in /usr/local/share/scheme/

You can also use "build" rather than "install", but only chicken's
compiled version will work when the user's current working directory
isn't the snow2 source directory.


Running
=======

```
$ snow2 -h
snow2 [arguments] <operation> '(library name)' ...
  <operation> can be "install" or "uninstall" or "list-depends" or "search"
  -r --repo <url>      Prepend to list of snow2 repositories.
  -s --symlink         Make symlinks to a repo's source files.
  -v --verbose         Print more.
  -h --help            Print usage message.
```


```
$ snow2 search hello
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

See <a href="https://github.com/sethalves/snow2-test-chicken">snow2-test-chicken</a> or <a href="https://github.com/sethalves/snow2-test-chibi">snow2-test-chibi</a> for more examples.
