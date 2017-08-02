# Overlord

Overlord is an experimental build/module system for Common Lisp,
inspired by [Redo][] and [Racket][].

Overlord addresses three problems which might seem unrelated, but
which, on closer examination, turn out to the same problem:

1. It provides a module system for implementing *languages as
   libraries* (similar to [Racket][]).

2. It provides a powerful general-purpose build system (a superset
   of [Make][], similar to [Redo][]).

3. It lets you reproducibly specify the desired state of a Lisp system
   which is to be saved as an image.

## Advice for users

*Overlord is experimental*. For the most part, trying to document the
API at this stage would be futile. Instead, this README discusses the
concepts behind Overlord. If you’re looking for the current syntax,
consult the [test suite](tests.lisp) and the [files it uses](tests/).

(If you are interested in reading the code, the substance is
in [impl.lisp](impl.lisp); the rest is support.)

Before loading Overlord, it would be a good idea to make sure you are
running the latest version of [ASDF][].

Note that, to run the test suite, you will need to
download [Core Lisp][], and, if not on Windows, you must have the
`touch` program in your search path. (On Windows, Powershell is
used instead).

When I say “experimental”, I mean it. Anything may change at any time.
This code is not ready for use. It may not even be ready for release.

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
systems, sharing a [design][djb-redo] due
to [Daniel J. Bernstein][djb] (although he has never released an
implementation). In the space of build systems, Redo is a remarkable
local optimum: it is both very powerful and very simple. It is not the
most powerful build system possible, or the very simplest, but it
certainly provides the most leverage.

Overlord generalizes the idea of a build system so that its dependency
graph can include both files on disk and state in the Lisp system. It
manages the state stored by Lisp itself, in symbols, and provides ways
to manage other kinds of state. This generalized build system is
modeled on Redo.

(Build systems manage state in the file system. In Common Lisp, we have
a bundle of state which, in many ways, resembles a file system: a set
of persistent, mutable locations with first-class, hierarchical
addresses. Symbols have value cells and function cells (mutable
locations); they have property lists; they are used as keys in
arbitrary namespaces (more locations); they can be persisted, in fasl
files and in saved images; and they are each uniquely addressed with a
path (package name, symbol name).)

The most obvious, but least important, difference between Overlord and
Redo in practice is that Redo uses shell scripts, while Overlord’s
“scripts” are written in Lisp. (It is unimportant because, after all,
you can run shell commands from Lisp, or somehow call Lisp from the
shell.) On the one hand, embedding shell syntax in Lisp is clumsy; on
the other hand, Lisp special variables are much superior to any
shell-based means for passing information between parent and child
scripts. (See §5.4.2 in [Grosskurth 2007][Grosskurth].)

The *important* difference is that Overlord uses *two* scripts per
target: one for building the target, and another for computing its
dependencies. This (mostly) replaces the need to maintain a database
of dependencies.

## CLI

Overlord has basic integration with the command line.

It is possible to run Overlord using either [cl-launch][]
or [Roswell][]. The cl-launch script is in `cl-launch/overlord`. The
Roswell script is in `roswell/overlord.ros`. They each use the same
syntax.

## Building with signals

One other potentially interesting aspect of Overlord’s implementation
of the Redo model is that walking the dependencies is done using
signals and handlers, rather than by recursing directly. That is, when
a script calls `depends-on`, all it does is *signal* that a dependency
exists. It is the default handler which is responsible for actually
building the dependency.

While the dependency graph of Overlord remains fundamentally
imperative, because of this separation, and because of the distinction
between build and dependency scripts, it should be possible to write
handlers that simply traverse the dependency graph (once it has been
built at least once), without necessarily having to actually build it.

## A word of warning

One thing that might not be obvious about Redo-style build systems is
that they afford unusually good opportunities for parallelism.
Although Overlord does not (yet) support parallelism, it tries to
discourage reliance on side effects by, whenever possible, randomizing
the order in which targets are built.

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
a direct imitation of Racket, however. It also draws on subsequent and
related work in Scheme module systems to create a module system that
is fundamentally dynamic.

## Modules

A Overlord module is a *file* in a *language*. Overlord supports a
Racket-like hash-lang (`#lang`) syntax, but in Overlord the language
of a module can also be specified as part of the import syntax. Since
the language is not an inherent part of the file, the same file can be
loaded as a module in more than one language. And each language-file
combination gets its own, completely independent module.

Overlord is very liberal about what can be a module. In Overlord, any
value can be a module – a string, a function, a hash table, anything –
and any module can provide exports as long as it specializes certain
generic functions, like `module-ref`.

(Most of the time, however, what you want
is [`simple-module`](#simple-modules).)

## Languages

In Racket, languages are defined in two steps. In Racket, a language
is a module. This module defines a reader, which returns syntax, and an
expander, which gives that syntax meaning.

That can’t work in Common Lisp, where meaning is assigned at read
time, when a symbol is interned in one or another package.

In Overlord, a language is just a package. The package exports a
reader and an expander. The symbol named `read-module` is the *package
reader*. The symbol named `module-progn` is the *package expander*.

The important thing: when the package’s reader is called, that same
package is also bound as the *current* package. It is then the
responsibility of the reader to make sure any symbols it reads in, or
inserts into the expansion, are interned in the correct package. (There
is a shortcut for this, `overlord:reintern`.)

(There is one exception to the rule of *language=package*. If another
package exists, having the same name, but ending in `-user`, and this
other package inherits from the original package, then this *user
package* is the package that is made current while reading (and
expanding). E.g. a file beginning with `#lang cl` would actually be
read in using the `cl-user` package, not the `cl` package itself.)

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

## Defining languages

Any package can be used as a hash lang, as long as its name is limited
to certain characters (`[a-zA-Z0-9/_+-]`). Of course this name can
also be a nickname.

(Note that resolution of package names is absolute, even in a Lisp
implementation that supports [package-local nicknames][].)

It is strongly recommended, although not required, that your language
package inherit from `overlord/shadows` rather than from `cl`. The
result is the same, except that `overlord/shadows` shadows Common
Lisp’s binding and definition forms so they can, in turn, be shadowed
in further language implementations.

The package must at least export a binding for one of `read-module`,
for direct use, or `module-progn`, for use with a meta-language.
Preferably, it would export both.

There are other special exports. For example, if the package has a
function binding for `module-static-exports`, it is used,
unsurprisingly, to statically determine the module’s exports.

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

The one exception is macros. A Racket-style single namespace for
run-time bindings and macros would not make sense in Overlord where
modules can be dynamically reloaded.

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

Overlord’s syntax for import and export supports macros. Unlike in
Racket, macros must be imported explicitly as macros, because
implicitly distinguishing functions and macros does not make sense
when it is possible to reload modules.

I am not sure that support for macros in the module system is useful.
In Racket, where languages and modules are the same thing, modules
must export macros as language extensions. In Overlord, modules are
one thing, and languages are something else – packages.

That said, I could hardly call the module system “Racket-inspired”
with a straight face if it didn’t support exporting macros from
modules.

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

- Lots of tests.
- More portability testing.
- Leverage UIOP more.
- Source locations for functions in embedded languages.
- More convenient shell command syntax.
- Thread safety (and eventually parallelism).
- Fewer dependencies.
- Improve the Emacs integration ([Prototype](elisp/overlord.el)).
- Improve the CLI. (Look in roswell/ and cl-launch/ for prototypes.)
- Interfaces for modules?
- Isolate file-system side effects (cf. [Boot][]).
- Using build directories (cf. [ASDF][]).

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

<!-- NB Don’t remove links, even if they’re not currently being used.
You might want them again later. -->


<!-- Local Variables: -->
<!-- compile-command: "pandoc README.md -o README.html" -->
<!-- End: -->
