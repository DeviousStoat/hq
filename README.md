# hq

Simple cli html nodes filtering tool

## Installation

Clone the project and install with cabal.

```sh
cabal install
```

## Usage

```sh
hq - Simple html nodes filtering tool

Usage: hq [SELECTOR] [-a|--attribute NAME] [-b|--before SELECTOR]
          [-a|--after SELECTOR] [(-t|--take N) | (-r|--take-rev N)]
          [-R|--remove SELECTOR] [-T|--text] [-N|--no-text]

  Filters html nodes by selector. `SELECTOR` is a small subset of CSS selectors.
  It only supports the `<tag>[<attr_name><op><attr_value> ...]` syntax. `op` can
  be `=`, `^=`, `$=`, `*=`, `~=`.

Available options:
  SELECTOR                 Selects nodes that match `SELECTOR`
  -a,--attribute NAME      Takes the attributes `NAME`'s values for each node
  -b,--before SELECTOR     Selects nodes before `SELECTOR`
  -a,--after SELECTOR      Selects nodes after `SELECTOR`
  -t,--take N              Takes `N` nodes
  -r,--take-rev N          Take `N` nodes from the end
  -R,--remove SELECTOR     Removes nodes that match `SELECTOR`
  -T,--text                Prints only text nodes
  -N,--no-text             Prints only non-text nodes
  -h,--help                Show this help text
```
