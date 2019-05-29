# Overlord

Overlord is a build system written in Common Lisp.

It has the features you would expect of any reasonably modern build
system: rules with multiple outputs, parallel builds, immunity to
clock issues, and dynamic dependencies.

It also solves certain problems specific to Lisp programming (namely,
it lets you you reproducibly specify the desired state of a Lisp
system which is to be saved as an image.)

For more discussion of the thinking behind Overlord and how it relates to other build systems, [consult the wiki][wiki].

## Advice for users

Note that, to run the test suite, you will need to
download [Core Lisp][], and, if not on Windows, you must have the
`touch` program in your search path. (On Windows, Powershell is
used instead).

Overlord stores its persistent data structures in a cache directory.
On Linux, this is `$XDG_CACHE_HOME/overlord`. The data structures
stored there are versioned. It might worth checking the cache
directory from time to time to delete obsolete files.

Overlord is developed and tested on Clozure and SBCL. In the future it
may officially support other Lisp implementations, but that is not a
priority.

## Examples

Here are some projects that make direct use of Overlord:

1. [cl-https-everywhere][]. In-process [HTTPS Everywhere][] rulesets,
   automatically fetched from the HTTPS Everywhere repository and
   compiled into Lisp code.

2. [Proctor][]. Proctor treats tests as build targets, allowing you to
   precisely specify their dependencies and re-run tests only when
   necessary.

3. [Vernacular][]. Provides a module system for embedding languages,
   with arbitrary syntaxes, into Common Lisp systems.

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->

[Lisp1.5]: http://www.softwarepreservation.org/projects/LISP/lisp15_family#Lisp_15_Programmers_Manual_
[phase separation]: http://www.phyast.pitt.edu/~micheles/scheme/scheme21.html
[language tower]: www.phyast.pitt.edu/~micheles/scheme/scheme22.html
[ASDF]: https://common-lisp.net/project/asdf/
[Racket]: https://racket-lang.org/
[Redo]: https://github.com/apenwarr/redo
[implicit phasing]: http://www.cs.indiana.edu/~dyb/pubs/implicit-phasing.pdf
[burgled-batteries]: https://github.com/pinterface/burgled-batteries
[Bosom Serpent]: http://github.com/ruricolist/bosom-serpent
[yesql]: https://github.com/krisajenkins/yesql
[cl-yesql]: http://github.com/ruricolist/cl-yesql
[HTTPS Everywhere]: https://github.com/EFForg/https-everywhere
[cl-https-everywhere]: http://github.com/ruricolist/cl-https-everywhere
[Instaparse]: https://github.com/Engelberg/instaparse
[Pseudoscheme]: https://github.com/sharplispers/pseudoscheme
[ragg]: http://www.hashcollision.org/ragg/
[shlex]: https://github.com/python/cpython/blob/master/Lib/shlex.py
[HCL]: http://www.jucs.org/jucs_16_2/embedding_hygiene_compatible_macros
[Shen]: http://www.shenlanguage.org/
[Serapeum]: https://github.com/ruricolist/serapeum
[at-exp]: https://docs.racket-lang.org/scribble/reader-internals.html
[CL-JavaScript]: http://marijnhaverbeke.nl/cl-javascript/
[Snowball]: http://snowballstem.org
[explicit renaming]: https://doi.org/10.1145/1317265.1317269
[Core Lisp home]: http://www.p-cos.net/core-lisp.html
[r6rs-imports]: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_chap_7
[package-local nicknames]: http://sbcl.org/manual/index.html#Package_002dLocal-Nicknames
[Grosskurth]: https://uwspace.uwaterloo.ca/handle/10012/2673
[apenwarr]: https://github.com/apenwarr/redo
[Ghuloum]: https://dl.acm.org/citation.cfm?id=1626863
[submodules]: https://dl.acm.org/citation.cfm?id=2517211
[YWIW]: https://dl.acm.org/citation.cfm?id=581486
[Racket Manifesto]: http://www.ccs.neu.edu/home/matthias/manifesto/
[ISLISP]: http://islisp.info/
[Core Lisp]: http://github.com/ruricolist/core-lisp
[SLIME]: http://common-lisp.net/project/slime/
[SLY]: https://github.com/joaotavora/sly
[Gasbichler]: https://pdfs.semanticscholar.org/8af5/fbb7988f83baa5a6c3e93e0db4c381abfc3a.pdf
[Bawden]: https://people.csail.mit.edu/alan/mtt/
[Frink]: https://frinklang.org
[LoL]: http://www.letoverlambda.com/
[djb-redo]: https://cr.yp.to/redo.html
[djb]: https://cr.yp.to/djb.html
[Beautiful Racket]: http://beautifulracket.com
[Maxima]: https://sourceforge.net/projects/maxima/
[ACL2]: https://www.cs.utexas.edu/users/moore/acl2/
[hopeless]: https://gist.github.com/samth/3083053
[parser generator]: http://cliki.net/parser%20generator
[Boot]: http://boot-clj.com
[Make]: https://www.gnu.org/software/make/
[Roswell]: https://github.com/roswell/roswell
[cl-launch]: http://cliki.net/cl-launch
[dev]: https://github.com/ruricolist/overlord/tree/dev
[Quicklisp]: https://www.quicklisp.org/beta/
[wiki]: https://github.com/ruricolist/overlord/wiki
[parallelism]: https://github.com/ruricolist/overlord/wiki/Parallelism-in-Overlord
[Proctor]: https://github.com/ruricolist/proctor
[Vernacular]: https://github.com/ruricolist/vernacular

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->


<!-- Local Variables: -->
<!-- compile-command: "pandoc README.md -o README.html" -->
<!-- End: -->
