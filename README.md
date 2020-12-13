# Introduction

This is way to write arbitrary instances
that should terminate in linear time with respect to budget,
or throw an error instead when they go over budget
and it is unclear how to terminate data structure.

## Usage

Just try:
```
import Test.LessArbitrary

data MyDataType = ...
  deriving (Generic)

instance LessArbitrary MyDataType
instance Arbitrary MyDataType where
  arbitrary = fasterArbitrary
```

## How it is done?

It is simply putting a monad that tracks the cost of constructors generated so far,
and throw an error when it goes too far. Goes for mutually recursive data structures
with at least one terminating constructor available.

Not to be used on non-terminating lazy data structures.

# Documentation build

To build detailed article on implementation, you would need:

* `pandoc`
* `xelatex` with standard science article packages
  - easiest to get as MacTeX on Mac
  - `apt-get install -y texlive-recommended` on Ubuntu
* `pandoc-hide-codeblocks` filter to hide unnecessary code blocks (optional)

# Hacker's guide

Entire Haskell code is in literate markdown `less-arbitrary.md`.
In order to work with it you will need `entangled` literate programming daemon
that synchronizes source files with the literate source.

It is bidirectional.

```
git clone https://github.com/entangled/entangled --branch develop
cd entangled;
cabal install # or stack install
entangled daemon
```
