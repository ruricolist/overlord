# Overlord

Overlord is an experimental build/module system for Common Lisp,
inspired by [Redo][] and [Racket][].

Overlord addresses three problems which might seem unrelated, but
which, on closer examination, turn out to the same problem:

1. It lets you reproducibly specify the desired state of a Lisp system
   which is to be saved as an image.

2. It provides a general-purpose build system (a superset of [Make][],
   inspired by [Redo][]).

3. It provides a module system for implementing *languages as
   libraries* (inspired by [Racket][]).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Overlord](#overlord)
    - [Advice for users](#advice-for-users)
        - [About Quicklisp](#about-quicklisp)
    - [Embedding languages](#embedding-languages)
    - [Language examples](#language-examples)
- [Overlord vs. ASDF](#overlord-vs-asdf)
- [Overlord vs. Redo](#overlord-vs-redo)
    - [CLI](#cli)
    - [A word of warning](#a-word-of-warning)
- [Overlord and Lisp images](#overlord-and-lisp-images)
- [Overlord vs. Racket](#overlord-vs-racket)
    - [Modules](#modules)
    - [Languages](#languages)
    - [Defining languages](#defining-languages)
    - [Imports and exports](#imports-and-exports)
    - [Simple modules](#simple-modules)
    - [Macro exports](#macro-exports)
- [Future work](#future-work)
- [Appendix: Overlord for Redoers](#appendix-overlord-for-redoers)
- [Appendix: Overlord for Racketeers](#appendix-overlord-for-racketeers)
    - [Macro exports](#macro-exports-1)

<!-- markdown-toc end -->

## Advice for users

*Overlord is experimental*. For the most part, trying to document the
API at this stage would be futile. Instead, this README discusses the
concepts behind Overlord. If you’re looking for the current syntax,
consult the [test suite](tests.lisp) and the [files it uses](tests/).

(If you are interested in reading the code, here are the more
interesting parts:

- [redo.lisp](redo.lisp) has the logic of the build system.
- [target.lisp](target.lisp) implements different kinds of targets.
- [db.lisp](db.lisp) implements the database.

The rest is support.)

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

Overlord supports building in parallel using threads. This feature is
not on by default. If you want to try building in parallel, execute:

    (setf (overlord:use-threads-p) t)

At this time building with threads on SBCL is not recommended. (In
particular, code that calls the Lisp compiler from multiple threads
may deadlock – this includes compiling modules.)

When I say “experimental”, I mean it. Anything may change at any time.

### About Quicklisp

Overlord is now in [Quicklisp][]. This does not mean Overlord is done:
it remains pre-alpha. But it does mean that bleeding-edge development
will now take place in a [separate `dev` branch][dev].

## Embedding languages

Overlord enables *languages as libraries*. Overlord languages have
several important properties:

1. Languages are *first-class*. Modules live in their own files, just
   like Lisp code, and are compiled into FASLs, just like Lisp
   code.

2. Languages can use *any syntax*. Unlike embedded DSLs, which are
   limited by what can be done with reader macros, full languages can
   use any parser they like.

3. Languages are *interoperable*. Lisp code can import modules written
   in embedded languages, and modules written in embedded languages
   can import other modules – even modules written in other languages.

4. Languages are *reusable*. Support for meta-languages allows
   different languages to share the same parser or for the same
   language to be written in more than one syntax.

One goal of the language implementation is to provide a reasonable
level of integration with Emacs. A prototype Emacs minor mode is
included in <elisp/overlord.el>. Using `overlord-minor-mode` lets you
recompile a module using `C-c C-k`, and lets you view the Lisp code
that the file expands into using `C-c RET` – just like an ordinary
macroexpansion.

## Language examples

Here are some example language embeddings:

1. [overlord/demo/js](demo/js.lisp). A simple demo language built
   on [CL-JavaScript][]. Shows how to convert a pre-existing CL language
   implementation to work with Overlord.

2. [Bosom Serpent][]. Shows how to wrap a foreign runtime (Python,
   using [burgled-batteries][]) as an Overlord module.

3. [cl-yesql][]. Lisp port of Clojure’s [yesql][]. Includes a parser,
   and shows how (and why) to load the same file in different
   languages.

4. [cl-https-everywhere][]. In-process [HTTPS Everywhere][] rulesets.
   Shows how to combine Overlord’s support for languages with direct
   use of the build system.

5. [Core Lisp][]. A hygiene-compatible implementation of the Lisp
   dialect [ISLISP][] (itself a conceptual subset of Common Lisp).
   Shows how to use Overlord to build “language towers.”

# Overlord vs. ASDF

Overlord expects to be used alongside ASDF, with ASDF responsible for
compiling and loading Lisp code, and Overlord doing everything else.

Overlord is mostly independent of ASDF. It still, however, needs ASDF
to resolve relative pathnames in Lisp code. Depending on ASDF in this
respect is probably not unreasonable, since making it easy to locate a
system’s files is what ASDF was designed for.

(Note that, while Overlord is mostly independent of ASDF the program,
it still depends on ASDF the project, because it builds on the UIOP
portability layer.)

(Note also that relying on ASDF means there is a discrepancy in the
handling of relative pathnames in Overlord modules vs. Lisp files.
Relative pathnames in Overlord modules are relative to the file that
contains the module, but relative pathnames in Lisp files are relative
to the base of the system they are loaded in. This is, unfortunately,
unlikely to change.)

# Overlord vs. Redo

[Redo][] is a build system. Actually, Redo is a family of build
systems, sharing a [design][djb-redo] due to [Daniel J.
Bernstein][djb].

In the design space of build systems, Redo is a remarkable local
optimum: it is both very powerful and very simple. It is not the most
powerful build system possible, or the very simplest, but it certainly
provides the most leverage.

Overlord generalizes the idea of a build system so that its dependency
graph can include both files on disk and state in the Lisp system. It
manages the state stored by Lisp itself, in symbols, and provides ways
to manage other kinds of state. This generalized build system is
modeled on Redo.

Note that, like Redo, but unlike ASDF or Make, Overlord does not care
about the ordering of timestamps. Timestamps (along with other
metadata) are stored in the database, and a file or other target is
considered changed as long as the timestamp is *different*, regardless
of whether it is newer or older than other timestamps.

## CLI

Overlord has basic integration with the command line.

It is possible to run Overlord using either [cl-launch][]
or [Roswell][]. The cl-launch script is in `cl-launch/overlord`. The
Roswell script is in `roswell/overlord.ros`. They each use the same
syntax.

## A word of warning

One thing that might not be obvious about Redo-style build systems is
that they afford unusually good opportunities for parallelism.
Overlord (conditionally) supports parallelism, but even when
parallelism is disabled, it tries to discourage reliance on side
effects by, whenever possible, randomizing the order in which targets
are built.

# Overlord and Lisp images

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

# Overlord vs. Racket

Overlord’s approach to making languages is inspired by Racket. Racket
is a language in the Lisp family, descended from Scheme. Its
distinction is its focus on making languages. An impressive amount of
thought, effort, and research has gone into making the whole Racket
environment act as a toolkit for making languages. Racket users are
both encouraged and expected to solve their problems by making
special-purpose languages; and Racket itself is implemented, from a
simple, Scheme-like core, as a tower of languages.

(If you want to investigate for yourself, you can find a gentle
introduction to making languages with Racket in [Beautiful Racket][].)

You might be wondering what making languages has to do with a build
system. The answer is that, once we have a build system that allows
Lisp bindings as targets, the idea of a language falls out naturally.
The most abstraction a file-to-file build system can support is a
*pattern* – an abstract relationship between two files. But what is an
abstract relationship between a file and a Lisp binding? The
relationship is a *language*; the Lisp object is a *module*.

Overlord models its support for making languages on Racket. It is not
a direct imitation of Racket, however. In particular, the goal of Overlord is to provide a module system that is fundamentally dynamic at the level of modules: it should always be possible to redefine a module, just as it is always possible (in Lisp itself) to redefine a function.

## Modules

A Overlord module is a *file* in a *language*. The language can be
specified in two ways.

The language can be specified as part of the file itself, with a
special first line. The special first line looks like this:

    #lang my-lang
    ....

This is called (following Racket) a *hash lang*.

The language of a module can also be specified as part of the import
syntax. Since the language is not an inherent part of the file, the
same file can be loaded as a module in more than one language. And
each language-file combination gets its own, completely independent
module.

## Languages

In Overlord, a language is just a package. The package exports a
reader and an expander. The symbol named `read-module` is the *package
reader*. The symbol named `module-progn` is the *package expander*.

The important thing: when the package’s reader is called, that same
package is also bound as the *current* package. It is then the
responsibility of the reader to make sure any symbols it reads in, or
inserts into the expansion, are interned in the correct package.
(There is a shortcut for this, `overlord:reintern`.)

(There is one exception to the rule of *language=package*. If another
package exists, having the same name, but ending in `-user`, and this
other package inherits from the original package, then this *user
package* is the package that is made current while reading (and
expanding). E.g. a file beginning with `#lang cl` would actually be
read in using the `cl-user` package, not the `cl` package itself.)

Note that the reader is responsible for returning a single form, which
is the module. That is, the form returned by the package reader should
already be wrapped in the appropriate `module-progn`. The exported
binding for `module-progn` is *only* looked up when the language is
being used as the expander for a meta-language.

(Meta-languages are for language authors who want to reuse an existing
syntax.)

## Defining languages

Any package can be used as a hash lang, as long as its name is limited
to certain characters (`[a-zA-Z0-9/_+-]`). Of course this name can
also be a nickname.

(Note that resolution of package names is absolute, even in a Lisp
implementation that supports [package-local nicknames][].)

It is recommended, although not required, that your language package
inherit from `overlord/cl` rather than from `cl`. The result is the
same, except that `overlord/cl` globally shadows Common Lisp’s binding
and definition forms so they can, in turn, be shadowed locally by
language implementations.

The package must at least export a binding for one of `read-module`,
for direct use, or `module-progn`, for use with a meta-language.
Preferably, it would export both.

If the syntax of your language makes it possible to determine exports
statically, you should also define and export `static-exports`. If
your language defines `static-exports`, then Overlord can statically
check the validity of import forms.

(This also has implications for phasing. If your language *doesn’t*
provide a `static-exports` binding, then the only way Overlord can
expand a request to import *all* bindings from a module is by loading
that module *at compile time* to get a list of its exports.)

## Imports and exports

What Overlord imports and exports are not values, but bindings. Bindings
are indirect (and immutable): they refer to the module, rather than to
the value of the export. This allows for modules to be reloaded at any
time. It is even possible to unload modules.

The overhead of this indirection is very low, but when necessary it
can be avoided.

Note that exports in Overlord, with one exception, form a single
namespace. This is in order to keep the notation for imports simple.
Importing from a language with multiple namespaces into a language
with multiple namespaces would create a Cartesian product problem.

The one exception is macros. A single namespace for run-time bindings
and macros would not make sense in Overlord where modules can be
dynamically reloaded.

Because Overlord imports bindings rather than values, modules are
always loaded lazily. A module is never actually loaded until a
function imported from it is called, or a variable imported from it is
looked up.

Finally, Overlord allows local imports: imports that only take effect
within the body of a `with-imports` form.

The combination of lazy loading and local imports may mean that, in
some cases, needless imports are minimized. For example, a module that
is only used inside of a macro might only be loaded when the macro is
expanded at compile time. However, this does not apply when saving
images: all known modules are loaded before the image is saved. The
real effect of pervasive lazy loading is that, since you do not know
when, or in what order, modules will be loaded, you must not rely on
load-time side effects.

## Simple modules

Most of the time, your language’s package expander will return a
`simple-module` form.

    (overlord:simple-module (#'moo)
      (defun make-moo (o)
        (concat "M" (make-string o :initial-element #\o)))

      (defun moo (&optional (o 2))
        (print (make-moo o))))

This exports a single name, `moo`, bound to a function that says “Moo”
with a varying amount of “oo”.

What makes simple modules simple is that they cannot export macros. If
you do want to export macros, you need something more complex (see
below).

The `simple-module` form is is built on the support for internal
definitions in [Serapeum][] (the `local` macro), and shares its
limitations with regard to the precedence of macro definitions. Macro
definitions must precede all function or variable definitions, and all
expressions.

## Macro exports

Overlord’s syntax for import and export supports macros.

The ability to export macros from modules is not useful in itself. It
only becomes useful in the presence of certain forms of macro hygiene.
After experimenting with different ways to do this, I have concluded
that the correct thing to do, if you want your language to be able to
export macros, is to embed a hygiene-compatible language in Lisp, and
then compile your language to that.

I’m not being flippant. Embedding a hygiene-compatible language in CL
is not just doable; it’s [already been done][HCL]. As a proof of
concept, I have converted Pascal’s Costanza’s hygiene-compatible
implementation of [ISLISP][] in Common Lisp
(“[Core Lisp][Core Lisp home]”) to work with Overlord’s module system.
This version of Core Lisp lives in [its own repository][Core Lisp].

How macro exports are supported is one aspect of the Overlord module
system that is very likely to change.

# Future work

- Lots more tests.
- Multiple outputs from one target.
- Use sub-second timestamps on file systems that support them.
- Better source locations for functions in embedded languages.
- Better support parallelism on SBCL.
- Improve the Emacs integration ([Prototype](elisp/overlord.el)).
- Improve the CLI. (Look in roswell/ and cl-launch/ for prototypes.)
- Multiple database backends.
- Use Overlord to build ASDF systems more correctly.
- Use Overlord to build ASDF systems in parallel.

Things I might or might not do, but sure would like to link to if
someone else did them.

- A EBNF parser generator like [ragg][] or [Instaparse][].
- An [at-exp][] meta-language.
- A working `#lang scheme`. (Compare [Pseudoscheme][]).
- A language for literate programming.
- A language with an ML-style type system.
- `#lang shen`.
- The [Snowball][] language.
- A unit-aware language (compare [Frink][]).
- Any language or meta-language you care to implement.

# Appendix: Overlord for Redoers

| Redo          | Overlord      |
| ------------- | ------------- |
| redo          | build         |
| redo-ifchange | depends-on    |
| redo-ifcreate | depends-not   |

Build systems manage state in the file system. In Common Lisp, we have
a bundle of state which, in many ways, resembles a file system: a set
of persistent, mutable locations with first-class, hierarchical
addresses. Symbols have value cells and function cells (mutable
locations); they have property lists; they are used as keys in
arbitrary namespaces (more locations); they can be persisted, in fasl
files and in saved images; and they are each uniquely addressed with a
path (package name, symbol name).

The obvious difference between Overlord and Redo in practice is that
Redo uses shell scripts, while Overlord’s “scripts” are written in
Lisp. (It is unimportant because, after all, you can run shell
commands from Lisp, or somehow call Lisp from the shell.) On the one
hand, embedding shell syntax in Lisp is clumsy; on the other hand,
Lisp special variables are much superior to any shell-based means for
passing information between parent and child scripts. (See §5.4.2 in
[Grosskurth 2007][Grosskurth].)

The important thing to remember about Overlord is that, because the
goal of Overlord is to maintain the consistency of the whole Lisp
system, there is only one project. Targets are resolved (i.e. relative
pathnames are made absolute) at compile time, not run time, relative
to the base inferred for the package which is current at the time they
are defined. And because all targets are absolute, the directory which
is current at the time the build takes place does not matter.

The fact that there is only one project also changes how patterns are
handled. Since patterns do not belong to a particular project, they
need another form of namespacing. Accordingly patterns are given names
(when they are defined with `defpattern`) and must be invoked by name.

All that said, Overlord is a surprisingly faithful implementation of
Redo. It differs from Redo principally in having a very different idea
of what can be a target.

Like most practical Redo implementations, Overlord does not (by
default) hash files to determine if they have changed. The timestamp
of the file, however, is treated as a hash-like value: the file is
rebuilt whenever it changes. This avoids both the problems of hashing
(slow) and the problems of timestamp ordering (unreliable clocks).

One way Overlord deviates from the Redo model is in how it decides
what is or is not a target. In Redo, this depends on the state of the
file system – a prerequisite is a target under two conditions: it does
not exist, or it exists and has an entry in the database. In Overlord,
however, the database is discarded with every time the version of
Overlord is incremented. The same goes for the Lisp implementation.
You could easily end up in a condition where a target is treated as a
source file because it exists, and has no entry in the newly created
database. So, instead of relying on the database to determine what is
or is not a target, Overlord treats an existing X as a target if and
only if there is a build script for X. (If X does not exist, then it
is still always treated as a target.)

Another way Overlord deviates from the Redo model is in how it builds
file targets. While it is possible to opt in to Redo-like behavior –
writing to a temp file that is atomically renamed to replace the
target – scripts can also overwrite the target directly. This allows
the script to choose *not* to write to the file if its output has not
changed (e.g. using `overlord:write-file-if-changed`). In this way we
achieve the same results as `redo-stamp` – the ability of a target to
declare itself unchanged – without complicating the underlying model.

# Appendix: Overlord for Racketeers

In Racket, languages are defined in two steps. In Racket, a language
is a module. This module defines a reader, which returns syntax, and an
expander, which gives that syntax meaning.

That can’t work in Common Lisp, where meaning is assigned at read
time, when a symbol is interned in one or another package.

In Racket the expander is entirely responsible for giving the code
meaning. In Overlord the expander has less to do, because the code has
been assigned meaning at read time, but it is still important for
lexical bindings and whole-program transformations.

Where in Racket you would write

    (module my-module MY-LANG forms ...)

to wrap FORMS with the binding of `#%module-begin` from MY-LANG, in
Overlord you just write:

    (MY-LANG:module-progn forms....)

Although for convenience you can write

    (overlord:module-progn-in :MY-LANG forms...)

But this just delays resolving `module-progn` from read time to
macro-expansion time.

Note that the reader is responsible for returning a single form, which
is the module. That is, the form returned by the package reader should
already be wrapped in the appropriate `module-progn`. The exported
binding for `module-progn` is *only* looked up when the language is
being used as the expander for a meta-language.

Meta-languages are for language authors who want to reuse an existing
syntax. E.g., if you want to define a language that uses
s-expressions, in Racket you can write:

    #lang s-exp my-lang

In Overlord you can do something similar:

    #lang overlord/s-exp my-lang

What happens here is simply that the `overlord/s-exp` language finds the
package `my-lang` and binds it to `*package*` *before* it starts
reading in forms.

## Macro exports

Unlike in Racket, macros must be imported explicitly as macros,
because implicitly distinguishing functions and macros does not make
sense when it is possible to reload modules.

I am not sure that support for macros in the module system is useful.
In Racket, where languages and modules are the same thing, modules
must export macros as language extensions. In Overlord, modules are
one thing, and languages are something else – packages.

That said, I could hardly call the module system “Racket-inspired”
with a straight face if it didn’t support exporting macros from
modules.

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
[Bosom Serpent]: http://github.com/TBRSS/bosom-serpent
[yesql]: https://github.com/krisajenkins/yesql
[cl-yesql]: http://github.com/TBRSS/cl-yesql
[HTTPS Everywhere]: https://github.com/EFForg/https-everywhere
[cl-https-everywhere]: http://github.com/TBRSS/cl-https-everywhere
[Instaparse]: https://github.com/Engelberg/instaparse
[Pseudoscheme]: https://github.com/sharplispers/pseudoscheme
[ragg]: http://www.hashcollision.org/ragg/
[shlex]: https://github.com/python/cpython/blob/master/Lib/shlex.py
[HCL]: http://www.jucs.org/jucs_16_2/embedding_hygiene_compatible_macros
[Shen]: http://www.shenlanguage.org/
[Serapeum]: https://github.com/TBRSS/serapeum
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
[Core Lisp]: http://github.com/TBRSS/core-lisp
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
[dev]: https://github.com/TBRSS/overlord/tree/dev
[Quicklisp]: https://www.quicklisp.org/beta/

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->


<!-- Local Variables: -->
<!-- compile-command: "pandoc README.md -o README.html" -->
<!-- End: -->
