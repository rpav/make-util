# MAKE-UTIL

`MAKE-UTIL` is the extremely trivial way to generate a `util.lisp`
file for your project from some functions you might have lying
around.  Unlike [quickutil](http://quickutil.org/), which has lots of
functionality, this is underengineered to do a few simple things:

```lisp
(make-util '(my-system "util")
           :package 'some-package
           :symbols '(fun1 some-macro another-function))
```

This lets you copy *any currently-defined function* (or macro) to
`"util.lisp"` as defined by your ASDF for `my-system`.

**Note:** `MAKE-UTIL` is *not* a dependency.  Your users do not need
this.  You should almost certainly not include it in your ASDF.

Features:

* Copy any function or macro you have loaded!
* Includes compiler macros if defined.
* Basic deduplication, so you can use one package across projects.
* Package definition and optional symbol export.

**Not** features:

* Downloading utilities
* Distributing utilities
* Infrastructure
* Really anything not listed above

If you need any of these, you can of course use QuickUtil in
conjunction.

## Using

Start a basic utility file by adding it to your ASDF:

```lisp
(defsystem my-system
    ...
    :components
    (...
     (:file "util")))
```

You should probably make a blank file to start with, or ASDF will
complain.

Load any utilities you want to include (via `asdf:load-system` or
QuickLisp or whatnot).  Then run:

```lisp
(make-util '(my-system "util")
           :package 'name
           :symbols '(LIST-OF-STUFF...))
```

Where `LIST-OF-STUFF` is a list of symbols.  Do not function-quote
them (i.e. `#'symbol`), or similar, `make-util` will figure it out.

You're done.  You've got a `util.lisp` file with the utilities you
selected.  Similar to QuickUtil, at the top is the command you can use
to refresh or update the file.

## Notes and Details

`make-util` is exceedingly simple, and works like this:

* Get the source location for `SYMBOL`
* `READ` the location
* Pretty-print stuff to the file

It's incredibly simple and thus there are some possible caveats:

* If the source is not compiled, or hasn't been compiled since the
  source file changed, it will likely not load correctly
* There is some basic detection for things generated by macro,
  e.g. `(make-thing foo)`.  If you include `foo`, but not
  `make-thing`, you will get a warning.
* This does not ensure that dependencies are included.  If you import
  a complicated function which references other things, and those
  things are not loaded or also imported, you will likely have issues.

That said, this is for loading simple utility-like functions that
*you* almost certainly wrote.  You're responsible for what you
include.  This literally pulls source into yours.  Watch the licenses.

(The license for `make-util` is BSD.  I do not consider generated
files derivative works or part of `make-util` unless you include some
other part of `make-util` yourself, so attribution is not required.)

Also I've only tested this on CCL and SBCL.  I'm pretty sure importing
should work across the board, but `make-util` itself depends on
Swank's ability to find source, which may not be uniform across
platforms.

## Idiom

All functions are wrapped in `FBOUNDP` checks.  That means you
*probably* should make and maintain *one* utility package in *all*
your projects, e.g.:

```lisp
(make-util '(...) :package 'util.rpav :symbols '(...))
```

Using e.g. "UTIL.<github-name>" or something is probably a nice way to
go.  If two of your projects both have the same utility function(s)
defined, they can share code.

You can of course simply import into your project package as well.  In
this case, you *might* want to specify `:exportp nil`, to preclude
exports.  You can still export specific things in your `defpackage`.
