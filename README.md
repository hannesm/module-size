## module-size - analyse binary size of an ELF binary with debug symbols

This is a program that uses `nm` to analyse a binary, and uses the `data_start`/`data_end` and `code_start`/`code_end` markers to figure out the sizes taken up by individual OCaml modules.

The result is an incomplete sorted list of (slightly mangled) module names. Native C code (such as external primitives, or linked libraries) or any other where the markers do not exist are not output.

This program is not meant to give a precise answer to the question which OCaml modules/libraries are using lots of binary size, but is to be used as a rough indication.

Tested with OCaml 4.07.1. Use at your own risk ;)

Compilation: `dune build` - this requires some opam packages, as well as `nm` being installed.
