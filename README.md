ppx_drivers
===========

This package implements a generic "ppx driver", to be used with OCaml
"-ppx" command-line flag.  It does not itself implements any AST
processing.  Instead, it loads external plugings (using Dynlink) which
implement AST rewriters and applies them to the current module.

Plugins to be loaded can be specified in the source code:

  external ppx : unit = "my_plugin.cmo" "arg1" "arg2" ... "argn"

which must appear as a toplevel structure (resp. signature) item in
the compiled .ml (resp. mli) file. The first string identifies the
plugin file.  The native code version of ppx_driver automatically
rewrites .cmo/.cma suffixes to .cmxs.  Extra arguments need only by
provided if the loaded rewriter expects them.

It is also possible to refer to plugins on the command-line of
ppx_driver itself.  All arguments ending with .cmo/.cma/.cmxs are
considered as plugins, and arguments following a plugin (until the next
one) are considered to be its arguments.

Before the first plugin on the command-line, ppx_driver recognizes the
following arguments:

  -I <dir>    Add <dir> to the list of directories where plugins are looked up.
  -v          Enable a verbose mode

