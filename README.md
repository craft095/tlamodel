# Configuration files generation for TLA+/TLC

This tool takes care about boilerplate, which is required if you wish to work with TLC
from command line. Due to TLA+/TLC specifics, some settings can only be specified in CFG file and
some other must go into separate TLA file. If you use ToolBox, it is done by that tool.
But if you want to use CLI, these files must be provided by you. It annoyed me enough to develop
this small utility :)

## How to use

The idea is to collect all TLC-related settings in one file with CFG-like syntax and generate all
needed artifacts from this file. I tried to keep syntax as close as possible to those in CFG and
in Toolbox GUI. However, to avoid true TLA expression parsing I had to introduce delimiter between
statements (semicolon).

Configuration is specified in <filename>.tpl file:

```
\* Use TLA-style comments:
\* for single line and
(*
    for multiline
    comments
*)

MODULE MyModule ;                         \* The name of TLA module to model check (file name used by default)

CONSTANTS
  C0 <- 42                                \* scalar name and arbitrary TLA expression
  C1(x) <- x * x                          \* operator and arbitrary TLA expression
  C2 <- [model value] X                   \* a single model value
  C3 <- [model value] { Y, Z }            \* a set of model values
  C2 <- [model value] <symmetrical> {}    \* a symmetrical set of model valuese
  ;

SPECIFICATION Spec ;                      \* Specification name (mandatory)
CHECK_DEADLOCK TRUE ;                     \* Check for deadlock? (TRUE by default)
INVARIANT TypeOK Inv0 Inv1;               \* Invariants
PROPERTY NoStarvation Prop1;              \* Properties

```

Use it with:

```
tlatpl [-s|--search-path <path>] MyModule.tpl
```

You can use -s option multiple times, then MyModule.tpl will be search in all these paths (current
folder is always used first)

It generates to files:

* MCMyModule.tla
* MCMyModule.cfg

Given these files you can run TLC to check your specification:

```
java tlc2.TLC MCMyModule
```
