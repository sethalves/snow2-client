snow2-client
============

Client for snow2 repositories.

This is alpha-quality software.  It might delete your files!

snow2-client is experimental software for finding and installing
libraries which are portable between
<a href="http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf">r7rs</a>
schemes.  The
libraries are stored in repositories which are described in:

  http://trac.sacrideo.us/wg/wiki/Snow

Inspiration and some packages are taken from the first snow system:

  http://snow.iro.umontreal.ca/

Supported schemes are:

<a href="https://github.com/ashinn/chibi-scheme">Chibi</a>,
<a href="http://call-cc.org/">Chicken</a>,
<a href="https://code.google.com/p/foment/">Foment</a>,
<a href="http://practical-scheme.net/gauche/">Gauche</a>,
<a href="http://www.gnu.org/software/kawa/index.html">Kawa</a>,
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

Running under CHICKEN requires that some eggs be installed:

```
chicken-install srfi-19 srfi-27 srfi-29 srfi-37 srfi-95 http-client openssl udp r7rs ssax sxpath hmac sha1
```

Additionally, some packages assume you have certain chicken eggs installed.

### Foment

Build Foment from git.

### Gauche

Gauche 0.9.4 can run snow2.

### Kawa

Build kawa from <a href="http://www.gnu.org/software/kawa/Getting-Kawa.html#Getting-the-development-sources-using-SVN">svn</a>.

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
SCHEME=chicken or kawa, in which case it compiles the client
(to a native program or to java class files, respectively).


Running
=======

```
$ snow2 -h

snow2 [arguments] <operation> '(library name)' ...
  <operation> can be one of: install uninstall list-depends search
  -r --repo <url>                Add to list of snow2 repositories.
  -p --prepend-repo <url>        Prepend to built-in list of snow2 repositories.
  -v --verbose                   Print more.
  -h --help                      Print usage message.

For install operation:
  -d --destination <directory>   Set where to install packages.
  -s --symlink                   Make symlinks to a repo's source files.
  -l --link                      Make hard-links to a repo's source files.
  -t --test                      Install code needed to run tests.

Repository Maintenance:
  When the current directory is within a source repository, <operation>
  can also be one of: run-source-tests package upload check

Example: snow2 install '(snow hello)'
```

### Operations

#### install

The requested libraries will be made available as children of the
current directory or, if the --destination option is used, as children
of the indicated directory.  If the package holding the requested
libraries is located on an HTTP server, it will be downloaded to a
file in /tmp/ and untarred (and the tarball in /tmp will be removed).
If the repository is a directory on the local file system, the package
will be untarred from the tarball located in the repository's
directory.  If the repository is a local directory and the --symlink
option is used, the installed libraries will be symbolic links to the
source files in the local repository.  This can be useful when working
on changes to a library.

If no repository urls (or filesystem paths) are indicated on
the command-line, the default repository will be used.

  http://snow2.s3-website-us-east-1.amazonaws.com/

#### uninstall

This currently does nothing.

#### list-depends

The arguments of the (depends ...) clause of each package containing
the mentioned libraries will be printed.

#### search

Display a list of libraries which have names that are matched by
a substring search with the provided argument.

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


Managing Repositories
=====================
snow2-client can also help maintain repositories.

#### run-source-tests

For each package indicated (or all of them, if none are indicated)
the package metafile is checked for libraries with a use-for phase of
test.  Each of these libraries is loaded into an environment and
the procedure run-tests is called.  run-tests is expected to return #t
if all its tests pass.  Packages with failing tests will be listed at
the end.

This currently fails with the CHICKEN version of snow2-client, because
chicken's r7rs (import ...) doesn't automatically load .sld files.

#### package

Combine the information in the indicated package metafiles (or all)
with information from the define-library clause of the relevant .sld
files.  Rewrite index.scm and index.html.  Create .tgz files containing
the files referenced (directly or indirectly) in the (path ...) clauses
in the package metafile.

Some of the information in index.scm is automatically computed (checksums,
file-sizes, library dependencies, etc) and shouldn't be in the package
metafiles.  By-hand changes should not be made to index.scm (or index.html)
because they will be overwritten when "snow2 package" is run.

#### upload

Check the md5 sum of index.scm, index.html, and each .tgz file against
copies in the s3 bucket implied by the repository's (url ...) clause
in index.scm.  For any mismatches, upload the local file to s3.  This
assumes you've already run "snow2 package".  It also assumes you've
created an index.css file and placed it in the top-level directory.

For aws credentials, the client looks for environment variables:
```
AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY
```

When it doesn't find those, it looks for the environment variable:
```
AWS_CREDENTIAL_FILE
```

Lastly, it looks for a file called: "/etc/aws/s3-*bucket-name*"

For example, if the repository url (in index.scm) is
```
"http://snow2.s3-website-us-east-1.amazonaws.com/index.scm"
```
the file "/etc/aws/s3-snow2" would be checked.  If the credentials are in a file, the format should be:

```
  AWSAccessKeyId=...
  AWSSecretKey=...
```

For a css-file starting point, try

```
wget http://snow2.s3-website-us-east-1.amazonaws.com/index.css
```


#### check

Do some sanity checking of the source repository.


Source Repository Structure
===========================

snow2-client makes a variety of assumptions about the layout of
a local source repository.

* The entire repository is inside one top-level directory.
* The top-level directory contains a packages/ directory.
* The top-level directory contains an index.scm file.
* The index.scm file has at least (repository (url "http://...")).
* Each package in the source repository has a .package metafile in the packages/ directory.
* Libraries are contained in .sld files and each of these only contains one library.

Each package metafile should be of the form:

```
(package
 (name (some name))
 (url "something.tgz")
 ... library declarations ...
 )
```

Each library declaration should look something like:

```
 (library
  (path "something/some-library.sld")
  (version "1.0")
  (homepage "https://github.com/sethalves")
  (maintainers "Seth Alves <seth@hungry.com>")
  (authors "Seth Alves <seth@hungry.com>")
  (description "some library")
  (license BSD-style))
```

See <a href="https://github.com/sethalves/snow2-example-source-repository">https://github.com/sethalves/snow2-example-source-repository</a> for a small example repository.


Other Scheme Package Managers
=============================

http://gna.org/projects/spells

http://home.gna.org/dorodango/manual/

http://planet.racket-lang.org/

http://synthcode.com/scheme/common-scheme/doc/common-scheme.html

http://wiki.call-cc.org/eggs

http://www.gnu.org/software/guix/

http://www.schemespheres.org/
