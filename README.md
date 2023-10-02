[![Build Status][build-status]][actions]
[![split release on Hackage][hackage-img]][hackage]

[build-status]: https://github.com/byorgey/split/actions/workflows/haskell-ci.yml/badge.svg
[hackage-img]: https://img.shields.io/hackage/v/split.svg?logo=haskell
[hackage]: https://hackage.haskell.org/package/split
[actions]: https://github.com/byorgey/split/actions

List splitting
==============

`Data.List.Split` provides a wide range of strategies and a unified
combinator framework for splitting lists with respect to some sort of
delimiter.  See
<https://hackage.haskell.org/package/split/docs/Data-List-Split.html>
to get started using it.

Dependencies
============

There are no dependencies other than the base package.
`Data.List.Split` is currently tested with versions of GHC from 7.0 up
through 9.6.  It is completely Haskell2010 (probably also Haskell98)
compliant, so it probably builds with other compilers as well.

The tests in `Properties.hs` depend on `QuickCheck` >= 2.4, but you
don't need it in order to build the library itself, only to run the
tests.
