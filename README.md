# Overlord

Overlord is an experimental build system for Common Lisp,
inspired by [Redo][].

Overlord addresses two problems which might seem unrelated, but
which, on closer examination, turn out to the same problem:

1. It lets you reproducibly specify the desired state of a Lisp system
   which is to be saved as an image.

2. It provides a general-purpose build system (a superset of [Make][],
   inspired by [Redo][]).

Overlord expects to be used alongside ASDF, with ASDF responsible for
compiling and loading Lisp code, and Overlord doing everything else.

For more discussion of the thinking behind Overlord, how it relates to
Redo and other build systems, [consult the wiki][wiki].

## Advice for users

*Overlord is experimental*. For the most part, trying to document the
API at this stage would be futile. Instead, this README discusses the
concepts behind Overlord. If you’re looking for the current syntax,
consult the [test suite](tests.lisp) and the [files it uses](tests/).

Before loading Overlord, it would be a good idea to make sure you are
running the latest version of [ASDF][].

Note that, to run the test suite, you will need to
download [Core Lisp][], and, if not on Windows, you must have the
`touch` program in your search path. (On Windows, Powershell is
used instead).

Overlord stores its persistent data structures in a cache directory.
On Linux, this is `$XDG_CACHE_HOME/overlord`. The data structures
stored there are versioned. Since this version number is increasing
rapidly, it might worth checking the cache directory from time to time
to delete obsolete files.

Overlord is developed and tested on Clozure and SBCL. In the future it
may support other Lisp implementations, but that is not a priority.
Lisp implementations that do not support image-based persistence (e.g.
ECL) are unlikely to receive support.

When I say “experimental”, I mean it. Anything may change at any time.

### About Quicklisp

Overlord is now in [Quicklisp][]. This does not mean Overlord is done:
it remains pre-alpha. But it does mean that development will now take
place in a [separate `dev` branch][dev].

## Examples

Here are some examples of how to make direct use of Overlord:

1. [cl-https-everywhere][]. In-process [HTTPS Everywhere][] rulesets,
   automatically fetched from the HTTPS Everywhere repository and
   compiled into Lisp code.

2. [Proctor][]. Proctor treats tests as build targets, allowing you to
   precisely specify their dependencies and re-run tests only when
   necessary.

## Parallelism

One thing that might not be obvious about Redo-style build systems is
that they afford unusually good opportunities for parallelism.

Overlord supports building in parallel using threads. Targets can
request that their dependencies be built in parallel by using
`pdepends-on` instead of `depends-on`. However, threads are not
enabled by default; this feature is still an experiment within an experiment.

If you want to try building in parallel, execute:

    (setf (overlord:use-threads-p) t)

It is not recommended that you try parallelizing targets that call the
Lisp compiler. (This includes importing modules.) On SBCL the Lisp
compiler is protected by a global lock, and trying to use it from
multiple threads can result in deadlock.

Overlord only uses parallelism when it is explicitly requested, but
even without parallelism, it tries to discourage reliance on side
effects by, whenever possible, randomizing the order in which targets
are built.

# Freezing the Lisp image

During development, as targets are defined and re-defined, and rebuilt
or not rebuilt, the actual state of the Lisp world will drift away
from the one specified by Overlord’s dependency graph. Before dumping
an image such discrepancies must be resolved. It is obviously
undesirable, for example, for an image built on one machine to try to
lazy-load a module on another machine where the source of that module
is unavailable. (Actually it would be a disaster, since that source
file might be provided maliciously.)

Thus, before an image is saved, Overlord needs to do two things:

1. Finalize the state of the image by making sure that all defined
   targets have been built.

2. Disable itself.

If you use `uiop:dump-image` to save the image, you don’t need to do
anything; Overlord will finalize the state of the image, and disable
itself, automatically.

If you are using implementation-specific means to save an image,
however, you will need to arrange to call `overlord:freeze` before the
image is saved.

The default policy is to allow the restored image to be unfrozen, and
development to be resumed, by calling `overlord:unfreeze`. This is
probably what you want when, say, saving an image on a server. In
other scenarios, however — like delivering a binary – you may want to
strip the build system from the image entirely. This is possible by
changing Overlord’s “freeze policy”, using the `freeze-policy`
accessor.

    ;;; The default: can be reversed after
    ;;; loading the image by calling
    ;;; `overlord:unfreeze`.
    (setf (overlord:freeze-policy) t)

    ;;; Irreversible: before saving the
    ;;; image, Overlord should destroy its
    ;;; internal state.
    (setf (overlord:freeze-policy) :hard)

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
[Proctor]: https://github.com/ruricolist/proctor

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->


<!-- Local Variables: -->
<!-- compile-command: "pandoc README.md -o README.html" -->
<!-- End: -->
