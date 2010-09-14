{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split.Internal
-- Copyright   :  (c) Brent Yorgey 2008
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
-- Stability   :  experimental
-- Portability :  unportable (GADTs, Rank2Types)
--
-- Implementation module for "Data.List.Split", a combinator library
-- for splitting lists.  See the "Data.List.Split" documentation for
-- more description and examples.
--
-----------------------------------------------------------------------------

module Data.List.Split.Internals where

import Data.List (genericSplitAt)

-- * Types and utilities

-- | A splitting strategy.
data Splitter a = Splitter { delimiter        :: Delimiter a
                               -- ^ What delimiter to split on
                           , delimPolicy      :: DelimPolicy
                               -- ^ What to do with delimiters (drop
                               --   from output, keep as separate
                               --   elements in output, or merge with
                               --   previous or following chunks)
                           , condensePolicy   :: CondensePolicy
                               -- ^ What to do with multiple
                               --   consecutive delimiters
                           , initBlankPolicy  :: EndPolicy
                               -- ^ Drop an initial blank?
                           , finalBlankPolicy :: EndPolicy
                               -- ^ Drop a final blank?
                           }

-- | The default splitting strategy: keep delimiters in the output
--   as separate chunks, don't condense multiple consecutive
--   delimiters into one, keep initial and final blank chunks.
--   Default delimiter is the constantly false predicate.
--
--   Note that 'defaultSplitter' should normally not be used; use
--   'oneOf', 'onSublist', or 'whenElt' instead, which are the same as
--   the 'defaultSplitter' with just the delimiter overridden.
--
--   The 'defaultSplitter' strategy with any delimiter gives a
--   maximally information-preserving splitting strategy, in the sense
--   that (a) taking the 'concat' of the output yields the original
--   list, and (b) given only the output list, we can reconstruct a
--   'Splitter' which would produce the same output list again given
--   the original input list.  This default strategy can be overridden
--   to allow discarding various sorts of information.
defaultSplitter :: Splitter a
defaultSplitter = Splitter { delimiter        = DelimEltPred (const False)
                           , delimPolicy      = Keep
                           , condensePolicy   = KeepBlankFields
                           , initBlankPolicy  = KeepBlank
                           , finalBlankPolicy = KeepBlank
                           }

-- | A delimiter can either be a predicate on elements, or a list of
--   elements to be matched as a subsequence.
data Delimiter a where
  DelimEltPred :: (a -> Bool) -> Delimiter a
  DelimSublist :: Eq a => [a] -> Delimiter a

-- | Try to match a delimiter at the start of a list, either failing
--   or decomposing the list into the portion which matched the delimiter
--   and the remainder.
matchDelim :: Delimiter a -> [a] -> Maybe ([a],[a])
matchDelim (DelimEltPred p) (x:xs) | p x       = Just ([x],xs)
                                   | otherwise = Nothing
matchDelim (DelimEltPred _) [] = Nothing
matchDelim (DelimSublist []) xs = Just ([],xs)
matchDelim (DelimSublist _)  [] = Nothing
matchDelim (DelimSublist (d:ds)) (x:xs)
  | d == x = matchDelim (DelimSublist ds) xs >>= \(h,t) -> Just (d:h,t)
  | otherwise = Nothing

-- | What to do with delimiters?
data DelimPolicy = Drop      -- ^ Drop delimiters from the output.
                 | Keep      -- ^ Keep delimiters as separate chunks
                             --   of the output.
                 | KeepLeft  -- ^ Keep delimiters in the output,
                             --   prepending them to the following
                             --   chunk.
                 | KeepRight -- ^ Keep delimiters in the output,
                             --   appending them to the previous chunk.
  deriving (Eq, Show)

-- | What to do with multiple consecutive delimiters?
data CondensePolicy = Condense         -- ^ Condense into a single delimiter.
                    | KeepBlankFields  -- ^ Insert blank chunks
                                       --   between consecutive
                                       --   delimiters.
  deriving (Eq, Show)

-- | What to do with a blank chunk at either end of the list
--   (i.e. when the list begins or ends with a delimiter).
data EndPolicy = DropBlank | KeepBlank
  deriving (Eq, Show)

-- | Tag chunks as delimiters or text.
data Chunk a = Delim [a] | Text [a]
  deriving (Show, Eq)

-- | Internal representation of a split list that tracks which pieces
--   are delimiters and which aren't.
type SplitList a = [Chunk a]

-- | Untag a 'Chunk'.
fromElem :: Chunk a -> [a]
fromElem (Text as) = as
fromElem (Delim as) = as

-- | Test whether a 'Chunk' is a delimiter.
isDelim :: Chunk a -> Bool
isDelim (Delim _) = True
isDelim _ = False

-- | Test whether a 'Chunk' is text.
isText :: Chunk a -> Bool
isText (Text _) = True
isText _ = False

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

-- * Implementation

-- | Given a delimiter to use, split a list into an internal
--   representation with chunks tagged as delimiters or text.  This
--   transformation is lossless; in particular, @'concatMap' 'fromElem'
--   ('splitInternal' d l) == l@.
splitInternal :: Delimiter a -> [a] -> SplitList a
splitInternal _ [] = []
splitInternal d xxs@(x:xs) = case matchDelim d xxs of
                               -- special case for blank delimiter
                               Just ([], (r:rs)) -> Delim [] : Text [r] : splitInternal d rs
                               Just (match,rest) -> Delim match : splitInternal d rest
                               _                 -> x `consText` splitInternal d xs
  where consText z (Text c : ys) = Text (z:c) : ys
        consText z ys             = Text [z] : ys

-- | Given a split list in the internal tagged representation, produce
--   a new internal tagged representation corresponding to the final
--   output, according to the strategy defined by the given
--   'Splitter'.
postProcess :: Splitter a -> SplitList a -> SplitList a
postProcess s = dropFinal (finalBlankPolicy s)
              . dropInitial (initBlankPolicy s)
              . doMerge (delimPolicy s)
              . doDrop (delimPolicy s)
              . insertBlanks
              . doCondense (condensePolicy s)

-- | Drop delimiters if the 'DelimPolicy' is 'Drop'.
doDrop :: DelimPolicy -> SplitList a -> SplitList a
doDrop Drop l = [ c | c@(Text _) <- l ]
doDrop _ l = l

-- | Condense multiple consecutive delimiters into one if the
--   'CondensePolicy' is 'Condense'.
doCondense :: CondensePolicy -> SplitList a -> SplitList a
doCondense KeepBlankFields ls = ls
doCondense Condense ls = condense' ls
  where condense' [] = []
        condense' (c@(Text _) : l) = c : condense' l
        condense' l = (Delim $ concatMap fromElem ds) : condense' rest
          where (ds,rest) = span isDelim l

-- | Insert blank chunks between any remaining consecutive delimiters,
--   and at the beginning or end if the first or last element is a
--   delimiter.
insertBlanks :: SplitList a -> SplitList a
insertBlanks [] = [Text []]
insertBlanks (d@(Delim _) : l) = Text [] : insertBlanks' (d:l)
insertBlanks l = insertBlanks' l

-- | Insert blank chunks between consecutive delimiters.
insertBlanks' :: SplitList a -> SplitList a
insertBlanks' [] = []
insertBlanks' (d1@(Delim _) : d2@(Delim _) : l) = d1 : Text [] : insertBlanks' (d2:l)
insertBlanks' [d@(Delim _)] = [d, Text []]
insertBlanks' (c : l) = c : insertBlanks' l

-- | Merge delimiters into adjacent chunks according to the 'DelimPolicy'.
doMerge :: DelimPolicy -> SplitList a -> SplitList a
doMerge KeepLeft = mergeLeft
doMerge KeepRight = mergeRight
doMerge _ = id

-- | Merge delimiters with adjacent chunks to the right (yes, that's
--   not a typo: the delimiters should end up on the left of the
--   chunks, so they are merged with chunks to their right).
mergeLeft :: SplitList a -> SplitList a
mergeLeft [] = []
mergeLeft ((Delim d) : (Text c) : l) = Text (d++c) : mergeLeft l
mergeLeft (c : l) = c : mergeLeft l

-- | Merge delimiters with adjacent chunks to the left.
mergeRight :: SplitList a -> SplitList a
mergeRight [] = []
mergeRight ((Text c) : (Delim d) : l) = Text (c++d) : mergeRight l
mergeRight (c : l) = c : mergeRight l

-- | Drop an initial blank chunk according to the given 'EndPolicy'.
dropInitial :: EndPolicy -> SplitList a -> SplitList a
dropInitial DropBlank (Text [] : l) = l
dropInitial _ l = l

-- | Drop a final blank chunk according to the given 'EndPolicy'.
dropFinal :: EndPolicy -> SplitList a -> SplitList a
dropFinal _ [] = []
dropFinal DropBlank l = case last l of
                          Text [] -> init l
                          _ -> l
dropFinal _ l = l

-- * Combinators

-- | Split a list according to the given splitting strategy.  This is
--   how to \"run\" a 'Splitter' that has been built using the other
--   combinators.
split :: Splitter a -> [a] -> [[a]]
split s = map fromElem . postProcess s . splitInternal (delimiter s)

-- ** Basic strategies
--
-- $ All these basic strategies have the same parameters as the
-- 'defaultSplitter' except for the delimiters.

-- | A splitting strategy that splits on any one of the given
--   elements.  For example:
--
-- > split (oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","x","","y","","z","c","x","d"]
oneOf :: Eq a => [a] -> Splitter a
oneOf elts = defaultSplitter { delimiter = DelimEltPred (`elem` elts) }

-- | A splitting strategy that splits on the given list, when it is
--   encountered as an exact subsequence.  For example:
--
-- > split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]
--
--   Note that splitting on the empty list is a special case, which
--   splits just before every element of the list being split.  For example:
--
-- > split (onSublist "") "abc" == ["","","a","","b","","c"]
-- > split (dropDelims . dropBlanks $ onSublist "") "abc" == ["a","b","c"]
--
--   However, if you want to break a list into singleton elements like
--   this, you are better off using @'splitEvery' 1@, or better yet,
--   @'map' (:[])@.
onSublist :: Eq a => [a] -> Splitter a
onSublist lst = defaultSplitter { delimiter = DelimSublist lst }

-- | A splitting strategy that splits on any elements that satisfy the
--   given predicate.  For example:
--
-- > split (whenElt (<0)) [2,4,-3,6,-9,1] == [[2,4],[-3],[6],[-9],[1]]
whenElt :: (a -> Bool) -> Splitter a
whenElt p = defaultSplitter { delimiter = DelimEltPred p }

-- ** Strategy transformers

-- | Drop delimiters from the output (the default is to keep
--   them). For example,
--
-- > split (oneOf ":") "a:b:c" == ["a", ":", "b", ":", "c"]
-- > split (dropDelims $ oneOf ":") "a:b:c" == ["a", "b", "c"]
dropDelims :: Splitter a -> Splitter a
dropDelims s = s { delimPolicy = Drop }

-- | Keep delimiters in the output by prepending them to adjacent
--   chunks.  For example:
--
-- > split (keepDelimsL $ oneOf "xyz") "aazbxyzcxd" == ["aa","zb","x","y","zc","xd"]
keepDelimsL :: Splitter a -> Splitter a
keepDelimsL s = s { delimPolicy = KeepLeft }

-- | Keep delimiters in the output by appending them to adjacent
--   chunks. For example:
--
-- > split (keepDelimsR $ oneOf "xyz") "aazbxyzcxd" == ["aaz","bx","y","z","cx","d"]
keepDelimsR :: Splitter a -> Splitter a
keepDelimsR s = s { delimPolicy = KeepRight }

-- | Condense multiple consecutive delimiters into one.  For example:
--
-- > split (condense $ oneOf "xyz") "aazbxyzcxd" == ["aa","z","b","xyz","c","x","d"]
-- > split (dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","","","c","d"]
-- > split (condense . dropDelims $ oneOf "xyz") "aazbxyzcxd" == ["aa","b","c","d"]
condense :: Splitter a -> Splitter a
condense s = s { condensePolicy = Condense }

-- | Don't generate a blank chunk if there is a delimiter at the
--   beginning.  For example:
--
-- > split (oneOf ":") ":a:b" == ["",":","a",":","b"]
-- > split (dropInitBlank $ oneOf ":") ":a:b" == [":","a",":","b"]
dropInitBlank :: Splitter a -> Splitter a
dropInitBlank s = s { initBlankPolicy = DropBlank }

-- | Don't generate a blank chunk if there is a delimiter at the end.
--   For example:
--
-- > split (oneOf ":") "a:b:" == ["a",":","b",":",""]
-- > split (dropFinalBlank $ oneOf ":") "a:b:" == ["a",":","b",":"]
dropFinalBlank :: Splitter a -> Splitter a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

-- ** Derived combinators

-- | Drop all blank chunks from the output.  Equivalent to
--   @'dropInitBlank' . 'dropFinalBlank' . 'condense'@.  For example:
--
-- > split (oneOf ":") "::b:::a" == ["",":","",":","b",":","",":","",":","a"]
-- > split (dropBlanks $ oneOf ":") "::b:::a" == ["::","b",":::","a"]
dropBlanks :: Splitter a -> Splitter a
dropBlanks = dropInitBlank . dropFinalBlank . condense

-- | Make a strategy that splits a list into chunks that all start
--   with the given subsequence (except possibly the first).
--   Equivalent to @'dropInitBlank' . 'keepDelimsL' . 'onSublist'@.
--   For example:
--
-- > split (startsWith "app") "applyappicativeapplaudapproachapple" == ["apply","appicative","applaud","approach","apple"]
startsWith :: Eq a => [a] -> Splitter a
startsWith = dropInitBlank . keepDelimsL . onSublist

-- | Make a strategy that splits a list into chunks that all start
--   with one of the given elements (except possibly the first).
--   Equivalent to @'dropInitBlank' . 'keepDelimsL' . 'oneOf'@.  For
--   example:
--
-- > split (startsWithOneOf ['A'..'Z']) "ACamelCaseIdentifier" == ["A","Camel","Case","Identifier"]
startsWithOneOf :: Eq a => [a] -> Splitter a
startsWithOneOf = dropInitBlank . keepDelimsL . oneOf

-- | Make a strategy that splits a list into chunks that all end with
--   the given subsequence, except possibly the last.  Equivalent to
--   @'dropFinalBlank' . 'keepDelimsR' . 'onSublist'@.  For example:
--
-- > split (endsWith "ly") "happilyslowlygnarlylily" == ["happily","slowly","gnarly","lily"]
endsWith :: Eq a => [a] -> Splitter a
endsWith = dropFinalBlank . keepDelimsR . onSublist

-- | Make a strategy that splits a list into chunks that all end with
--   one of the given elements, except possibly the last.  Equivalent
--   to @'dropFinalBlank' . 'keepDelimsR' . 'oneOf'@.  For example:
--
-- > split (condense $ endsWithOneOf ".,?! ") "Hi, there!  How are you?" == ["Hi, ","there!  ","How ","are ","you?"]
endsWithOneOf :: Eq a => [a] -> Splitter a
endsWithOneOf = dropFinalBlank . keepDelimsR . oneOf

-- ** Convenience functions
--
-- These functions implement some common splitting strategies.  Note
-- that all of the functions in this section drop delimiters from
-- the final output, since that is a more common use case even
-- though it is not the default.

-- | Split on any of the given elements.  Equivalent to @'split'
--   . 'dropDelims' . 'oneOf'@.  For example:
--
-- > splitOneOf ";.," "foo,bar;baz.glurk" == ["foo","bar","baz","glurk"]
splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf = split . dropDelims . oneOf

-- | Split on the given sublist.  Equivalent to @'split'
--   . 'dropDelims' . 'onSublist'@.  For example:
--
-- > splitOn ".." "a..b...c....d.." == ["a","b",".c","","d",""]
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn   = split . dropDelims . onSublist

-- | Split on elements satisfying the given predicate.  Equivalent to
--   @'split' . 'dropDelims' . 'whenElt'@.  For example:
--
-- > splitWhen (<0) [1,3,-4,5,7,-9,0,2] == [[1,3],[5,7],[0,2]]
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = split . dropDelims . whenElt

-- | A synonym for 'splitOn'.
sepBy :: Eq a => [a] -> [a] -> [[a]]
sepBy = splitOn

-- | A synonym for 'splitOneOf'.
sepByOneOf :: Eq a => [a] -> [a] -> [[a]]
sepByOneOf = splitOneOf

-- | Split into chunks terminated by the given subsequence.
--   Equivalent to @'split' . 'dropFinalBlank' . 'dropDelims'
--   . 'onSublist'@.  For example:
--
-- > endBy ";" "foo;bar;baz;" == ["foo","bar","baz"]
--
--   Note also that the 'lines' function from "Data.List" is equivalent
--   to @'endBy' \"\\n\"@.
endBy :: Eq a => [a] -> [a] -> [[a]]
endBy = split . dropFinalBlank . dropDelims . onSublist

-- | Split into chunks terminated by one of the given elements.
--   Equivalent to @'split' . 'dropFinalBlank' . 'dropDelims' . 'oneOf'@.
endByOneOf :: Eq a => [a] -> [a] -> [[a]]
endByOneOf = split . dropFinalBlank . dropDelims . oneOf

-- | A synonym for 'sepBy' \/ 'splitOn'.
--
--   Note that this is the right inverse of the 'intercalate' function
--   from "Data.List", that is, @'intercalate' x . 'unintercalate' x
--   == 'id'@.  It is also the case that @'unintercalate' x
--   . 'intercalate' x@ is idempotent.  @'unintercalate' x
--   . 'intercalate' x@ is the identity on certain lists, but it is
--   tricky to state the precise conditions under which this holds.
--   (For example, it is not enough to say that @x@ does not occur in
--   any elements of the input list.  Working out why is left as an
--   exercise for the reader.)
unintercalate :: Eq a => [a] -> [a] -> [[a]]
unintercalate = sepBy

-- | Split into words, with word boundaries indicated by the given
--   predicate.  Satisfies @words === wordsBy isSpace@; equivalent to
--   @split . dropBlanks . dropDelims . whenElt@.  For example:
--
-- > wordsBy (=='x') "dogxxxcatxbirdxx" == ["dog","cat","bird"]
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy = split . dropBlanks . dropDelims . whenElt

-- | Split into lines, with line boundaries indicated by the given
--   predicate. Satisfies @lines === linesBy (=='\n')@; equivalent to
--   @split . dropFinalBlank . dropDelims . whenElt@.  For example:
--
-- > linesBy (=='x') "dogxxxcatxbirdxx" == ["dog","","","cat","bird",""]
linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy = split . dropFinalBlank . dropDelims . whenElt

-- * Other splitting methods

-- | @'splitEvery' n@ splits a list into length-n pieces.  The last
--   piece will be shorter if @n@ does not evenly divide the length of
--   the list.  If @n <= 0@, @'splitEvery' n l@ returns an infinite list
--   of empty lists.
--
--   Note that @'splitEvery' n []@ is @[]@, not @[[]]@.  This is
--   intentional, and is consistent with a recursive definition of
--   'splitEvery'; it satisfies the property that
--
--   @splitEvery n xs ++ splitEvery n ys == splitEvery n (xs ++ ys)@
--
--   whenever @n@ evenly divides the length of @xs@.
splitEvery :: Int -> [e] -> [[e]]
splitEvery i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- | A common synonym for 'splitEvery'.
chunk :: Int -> [e] -> [[e]]
chunk = splitEvery

-- | Split a list into chunks of the given lengths. For example:
--
-- > splitPlaces [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]
-- > splitPlaces [4,9] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
--
--   The behavior of @'splitPlaces' ls xs@ when @'sum' ls /= 'length' xs@ can
--   be inferred from the above examples and the fact that 'splitPlaces'
--   is total.
splitPlaces :: Integral a => [a] -> [e] -> [[e]]
splitPlaces is ys = build (splitPlacer is ys) where
  splitPlacer [] _ _ n      = n
  splitPlacer _ [] _ n      = n
  splitPlacer (l:ls) xs c n = let (x1, x2) = genericSplitAt l xs
                              in  x1 `c` splitPlacer ls x2 c n
