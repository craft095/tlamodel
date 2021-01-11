# Configuration files generation for TLA+/TLC

This small utility takes care about boilerplate, which is required if you wish to work with TLC
from command line. Due to TLA+/TLC specifics, some settings can only be specified in CFG file and
some other must go into separate TLA file. If you use ToolBox, it is done by that tool.
But if you want to use CLI, these files must be provided by you. It annoyed me enough to develop
this small utility :)

## How to use

The idea is to collect all TLC-related settings in one file with CFG-like syntax and generate all
needed artifacts from this file.

Configuration is specified in <filename>.tpl file:

```
\* Use TLA-style comments:
\* for single line and
(*
    for multiline
    comments
*)


MODULE MyModule ; \* The name of TLA module to model check

CONSTANTS
  C0 <- 42
  C1(x) <- x * x ;

SPECIFICATION Spec ;
INVARIANT TypeOK ;
PROPERTY NoStarvation ;

```

Use it with:

```
tlatpl [--search-path <path>] MyModule.tpl
```

It generates to files:

* MCMyModule.tla
* MCMyModule.cfg

Given these files you can run TLC to check your specification:

```
java tlc2.TLC MCMyModule
```
