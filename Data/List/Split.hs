-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split
-- Copyright   :  (c) Brent Yorgey, Louis Wasserman 2008
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable (GADTs, Rank2Types)
--
-- The "Data.List.Split" module contains a wide range of strategies
-- for splitting lists with respect to some sort of delimiter, mostly
-- implemented through a unified combinator interface.  The goal is to
-- be flexible yet simple.  Scroll past the Synopsis for usage,
-- examples, and detailed documentation of all exported functions.  If
-- you want to learn about the implementation, see
-- "Data.List.Split.Internal".
--
-- A darcs repository containing the source (including a module with
-- over 40 QuickCheck properties) can be found at
-- <http://code.haskell.org/~byorgey/code/split>.
--
-----------------------------------------------------------------------------
module Data.List.Split (

                       -- * Getting started
                       -- $started

                       -- * Convenience functions
                       -- $conv

                         splitOneOf
                       , splitOn
                       , splitWhen
                       , sepBy
                       , sepByOneOf
                       , endBy
                       , endByOneOf

                       , unintercalate

                       -- * Splitting combinators
                       -- $comb

                       , Splitter
                       , defaultSplitter
                       , split

                       -- ** Basic strategies
                       -- $basic

                       , oneOf
                       , onSublist
                       , whenElt

                       -- ** Strategy transformers
                       --
                       -- $ Functions for altering splitting strategy
                       --   parameters.
                       , dropDelims
                       , keepDelimsL
                       , keepDelimsR
                       , condense
                       , dropInitBlank
                       , dropFinalBlank

                       -- ** Derived combinators
                       --
                       -- $ Combinators which can be defined in terms of
                       --   other combinators, but are provided for
                       --   convenience.
                       , dropBlanks
                       , startsWith
                       , startsWithOneOf
                       , endsWith
                       , endsWithOneOf

                       -- * Other splitting methods
                       --
                       -- $ Other Useful splitting methods which are not
                       --   implemented using the combinator framework.
                       , splitEvery
                       , chunk
                       , splitPlaces
                       ) where

import Data.List.Split.Internals

-- $started
-- To get started, you should take a look at the functions 'splitOn',
-- 'splitWhen', 'sepBy', 'endBy', and other functions listed in the
-- section labeled \"Convenience functions\".  These functions
-- implement various common splitting operations, and one of them will
-- probably do the job 90\% of the time.  For example:
--
-- > > splitOn "x" "axbxc"
-- > ["a","b","c"]
-- > > splitOn "x" "axbxcx"
-- > ["a","b","c",""]
-- > > endBy ";" "foo;bar;baz;"
-- > ["foo","bar","baz"]
-- > > splitWhen (<0) [1,3,-4,5,7,-9,0,2]
-- > [[1,3],[5,7],[0,2]]
-- > > splitOneOf ";.," "foo,bar;baz.glurk"
-- > ["foo","bar","baz","glurk"]
--
-- If you want more flexibility, however, you can use the combinator
-- library in terms of which these functions are defined.  For more
-- information, skip to the section labeled \"Splitting Combinators\".

-- $conv
-- These functions implement some common splitting strategies.  Note
-- that all of the functions in this section drop delimiters from the
-- final output, since that is a more common use case even though it
-- is not the default.

-- $basic
-- All these basic strategies have the same parameters as the
-- 'defaultSplitter' except for the delimiter.

