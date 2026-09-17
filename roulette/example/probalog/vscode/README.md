# Probalog syntax highlighting for VS Code

A TextMate grammar for [Probalog](../Readme.md), the probabilistic Datalog
built on Roulette. It covers `.pdl` and `.probalog` files, and any file whose
first line is `#lang roulette/example/probalog` that no other extension has
already claimed.

## Install

Link this directory into the VS Code extensions directory and restart:

```
ln -s "$(pwd)" ~/.vscode/extensions/probalog
```

run from this directory. To build a `.vsix` instead:

```
npx @vscode/vsce package
code --install-extension probalog-0.1.0.vsix
```

## What it does and doesn't do

Coloring only. Everything semantic — diagnostics, hover, jump to binding,
rename — comes from
[racket-langserver](https://github.com/jeapostrophe/racket-langserver), which
runs Racket's Check Syntax over the expanded program. The language reports
source locations for parse errors and marks variable occurrences for Check
Syntax, so the langserver has real information to work with, including binding
arrows between a rule's variables.

A Probalog program saved as `.rkt` gets Racket's coloring rather than this
grammar, because VS Code picks a grammar by file type and the Racket extension
claims `.rkt`. The langserver still works on those files. Save as `.pdl` to get
both.

The scopes match the categories DrRacket's colorer uses for the same language,
so the two editors agree on what is a predicate, a variable, and a constant:

| Probalog             | scope                                    |
| -------------------- | ---------------------------------------- |
| `Edge`, `Path`       | `entity.name.function.probalog`          |
| `x`, `y`             | `variable.parameter.probalog`            |
| `"a"`                | `string.quoted.double.probalog`          |
| `0.5`                | `constant.numeric.probalog`              |
| `::`, `:-`           | `keyword.operator.probalog`              |
| `?`, `!`, `~`        | `keyword.control.probalog`               |
| `% comment`          | `comment.line.percentage.probalog`       |
