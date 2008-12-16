-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split
-- Copyright   :  (c) Brent Yorgey 2008
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
-- Stability   :  experimental
-- Portability :  unportable (GADTs, Rank2Types)
--
-- The "Data.List.Split" module ...
--
-----------------------------------------------------------------------------
module Data.List.Split (

                       -- * Splitting combinators

                         Splitter
                       , defaultSplitter
                       , split

                       -- ** Basic strategies
                       --
                       -- $ All these basic strategies have the same
                       --   parameters as the 'defaultSplitter' except
                       --   for the delimiters.
                       , oneOf
                       , onSublist
                       , whenElt

                       -- ** Strategy transformers
                       , dropDelims
                       , keepDelimsL
                       , keepDelimsR
                       , condense
                       , dropInitBlank
                       , dropFinalBlank

                       -- ** Derived combinators
                       , dropBlanks
                       , startsWith
                       , startsWithOneOf
                       , endsWith
                       , endsWithOneOf

                       -- ** Convenience functions
                       --
                       -- $ These functions implement some common
                       --   splitting strategies.  Note that all of
                       --   the functions in this section drop
                       --   delimiters from the final output, since
                       --   that is a more common use case even though
                       --   it is not the default.
                       , splitOneOf
                       , splitOn
                       , splitWhen
                       , sepBy
                       , sepByOneOf
                       , endBy
                       , endByOneOf

                       , unintercalate

                       -- * Other splitting methods
                       , splitEvery
                       , chunk
                       , splitPlaces
                       ) where

import Data.List.Split.Internals


