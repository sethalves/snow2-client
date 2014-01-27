snow2-client
============

clients for a snow2 repository

See:

  http://trac.sacrideo.us/wg/wiki/Snow <br>
  http://snow.iro.umontreal.ca/ <br>
  https://github.com/sethalves <br>

Supported schemes are:

<a href="http://synthcode.com/scheme/chibi/">Chibi</a>,
<a href="http://call-cc.org/">Chicken</a>,
<a href="http://practical-scheme.net/gauche/">Gauche</a>,
<a href="https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home">Sagittarius</a>


Requirements
============

Snow2 currently (2014-1-26) requires built-from-trunk versions of all 4
supported schemes.

Chicken requires that several eggs be installed, one from svn.

```
sudo chicken-install srfi-27 srfi-37 http-client
svn co https://code.call-cc.org/svn/chicken-eggs/release/4/r7rs/trunk r7rs
cd r7rs
sudo chicken-install
```


Building
========

```
sudo make SCHEME=<scheme> install
```

<scheme> can be any of chibi, chicken, gauche, sagittarius.  For example:

```
sudo make SCHEME=chicken install
```

snow2 will be placed in /usr/local/bin/

Some libraries will be placed in /usr/local/share/scheme/


Running
=======

```
$ snow2 search hello
(snow hello)
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