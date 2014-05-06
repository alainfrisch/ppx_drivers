ppx_drivers
===========

This package implements a generic ppx driver to be used with OCaml
"-ppx" command-line flag.  This driver dynamically loads other
Parsetree rewriters which are supposed to be installed as findlib
packages.

Rewriters to be applied can be specified either on the command-line or
directly in the source code.  Each rewriter is specified as the name
of its findlib package and can be passed some extra arguments.

Command-line syntax:

   ppx_driver [-v] pkg arg ... arg -- pkg arg ... arg -- ....


In-source syntax (as a top-level structure or signature item):

   [%%load_ppx "pkg" "arg" ... "arg"]

