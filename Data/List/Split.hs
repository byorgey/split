{-# LANGUAGE GADTs, Rank2Types, PatternGuards #-}
module Data.List.Split (
                         Splitter
                       , split
                       , oneOf
                       , onSublist
                       , whenElt
                       , keepDelims
                       , keepDelimsL
                       , keepDelimsR
                       , condense

                       , splitOn
                       , splitWhen
                       ) where

import Data.List (unfoldr)

data Splitter a = Splitter { delimiter        :: Delimiter a
                           , delimPolicy      :: DelimPolicy
                           , condensePolicy   :: CondensePolicy
                           , initBlankPolicy  :: EndPolicy
                           , finalBlankPolicy :: EndPolicy
                           }

defaultSplitter = Splitter { delimiter        = DelimEltPred (const False)
                           , delimPolicy      = Drop
                           , condensePolicy   = KeepBlankFields
                           , initBlankPolicy  = KeepBlank
                           , finalBlankPolicy = KeepBlank
                           }

data Delimiter a where
  DelimEltPred :: (a -> Bool) -> Delimiter a
  DelimSublist :: Eq a => [a] -> Delimiter a

matchDelim :: Delimiter a -> [a] -> Maybe ([a],[a])
matchDelim (DelimEltPred p) (x:xs) | p x       = Just ([x],xs)
                                   | otherwise = Nothing
matchDelim (DelimSublist []) xs = Just ([],xs)
matchDelim (DelimSublist _)  [] = Nothing
matchDelim (DelimSublist (d:ds)) (x:xs)
  | d == x = matchDelim (DelimSublist ds) xs >>= \(h,t) -> Just (d:h,t)
                                          -- $$ (fmap.first) (d:)
  | otherwise = Nothing

data DelimPolicy = Drop | Keep | KeepLeft | KeepRight

data CondensePolicy = Condense | KeepBlankFields

data EndPolicy = DropBlank | KeepBlank

data SplitElem a = Chunk [a] | Delim [a]
  deriving (Show, Eq)
type SplitList a = [SplitElem a]

fromElem :: SplitElem a -> [a]
fromElem (Chunk as) = as
fromElem (Delim as) = as

isDelim :: SplitElem a -> Bool
isDelim (Delim _) = True
isDelim _ = False

splitInternal :: Delimiter a -> [a] -> SplitList a
splitInternal _ [] = []
splitInternal d xxs@(x:xs) | Just (match,rest) <- matchDelim d xxs = Delim match : splitInternal d rest
                   | otherwise = x `consChunk` splitInternal d xs
  where consChunk x (Chunk c : ys) = Chunk (x:c) : ys
        consChunk x ys             = Chunk [x] : ys

split :: Splitter a -> [a] -> [[a]]
split s = postProcess s . splitInternal (delimiter s)

postProcess :: Splitter a -> SplitList a -> [[a]]
postProcess s = map fromElem
              . dropFinal (finalBlankPolicy s)
              . dropInitial (initBlankPolicy s)
              . mergeDelims (delimPolicy s)
              . dropDelims (delimPolicy s)
              . insertBlanks
              . condenseDelims (condensePolicy s)

-- Possibly drop delimiters.
dropDelims :: DelimPolicy -> SplitList a -> SplitList a
dropDelims Drop l = [ c | c@(Chunk _) <- l ]
dropDelims _ l = l

condenseDelims :: CondensePolicy -> SplitList a -> SplitList a
condenseDelims KeepBlankFields l = l
condenseDelims Condense l = condense' l
  where condense' [] = []
        condense' (c@(Chunk _) : l) = c : condense' l
        condense' l = (Delim $ concatMap fromElem ds) : condense' rest
          where (ds,rest) = span isDelim l

insertBlanks :: SplitList a -> SplitList a
insertBlanks [] = [Chunk []]  -- XXX ?
insertBlanks (d@(Delim _) : l) = Chunk [] : insertBlanks' (d:l)
insertBlanks l = insertBlanks' l

insertBlanks' :: SplitList a -> SplitList a
insertBlanks' [] = []
insertBlanks' (d1@(Delim _) : d2@(Delim _) : l) = d1 : Chunk [] : insertBlanks' (d2:l)
insertBlanks' [d@(Delim _)] = [d, Chunk []]
insertBlanks' (c : l) = c : insertBlanks' l

mergeDelims :: DelimPolicy -> SplitList a -> SplitList a
mergeDelims KeepLeft = mergeLeft
mergeDelims KeepRight = mergeRight
mergeDelims _ = id

mergeLeft :: SplitList a -> SplitList a
mergeLeft [] = []
mergeLeft ((Delim d) : (Chunk c) : l) = Chunk (d++c) : mergeLeft l
mergeLeft (c : l) = c : mergeLeft l

mergeRight :: SplitList a -> SplitList a
mergeRight [] = []
mergeRight ((Chunk c) : (Delim d) : l) = Chunk (c++d) : mergeRight l
mergeRight (c : l) = c : mergeRight l

dropInitial :: EndPolicy -> SplitList a -> SplitList a
dropInitial DropBlank (Chunk [] : l) = l
dropInitial _ l = l

dropFinal :: EndPolicy -> SplitList a -> SplitList a
dropFinal DropBlank l | Chunk [] <- last l = init l
dropFinal _ l = l

-- Combinators

oneOf :: Eq a => [a] -> Splitter a     -- split on any of these elements
oneOf elts = defaultSplitter { delimiter = DelimEltPred (`elem` elts) }

onSublist :: Eq a => [a] -> Splitter a  -- split on this exact subsequence
onSublist lst = defaultSplitter { delimiter = DelimSublist lst }

whenElt :: (a -> Bool) -> Splitter a
whenElt p = defaultSplitter { delimiter = DelimEltPred p }

keepDelims :: Splitter a -> Splitter a
keepDelims s = s { delimPolicy = Keep }

keepDelimsL :: Splitter a -> Splitter a
keepDelimsL s = s { delimPolicy = KeepLeft }

keepDelimsR :: Splitter a -> Splitter a
keepDelimsR s = s { delimPolicy = KeepRight }

condense :: Splitter a -> Splitter a
condense s = s { condensePolicy = Condense }

dropInitBlank :: Splitter a -> Splitter a
dropInitBlank s = s { initBlankPolicy = DropBlank }

dropFinalBlank :: Splitter a -> Splitter a
dropFinalBlank s = s { finalBlankPolicy = DropBlank }

-- now you can write things like
--
--   split (condense $ oneOf " ,") "abc,def   , gh"
--
-- which should evaluate to ["abc", "def", "gh"].

-- some convenience functions can be provided, such as...

splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf = split . oneOf

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn   = split . onSublist

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = split . whenElt

startsWith :: Eq a => [a] -> Splitter a
startsWith = dropInitBlank . keepDelimsL . onSublist

startsWithOneOf :: Eq a => [a] -> Splitter a
startsWithOneOf = dropInitBlank . keepDelimsL . oneOf

endsWith :: Eq a => [a] -> Splitter a
endsWith = dropFinalBlank . keepDelimsR . onSublist

endsWithOneOf :: Eq a => [a] -> Splitter a
endsWithOneOf = dropFinalBlank . keepDelimsR . oneOf

sepBy :: Eq a => [a] -> [a] -> [[a]]
sepBy = splitOn

sepByOneOf :: Eq a => [a] -> [a] -> [[a]]
sepByOneOf = splitOneOf

endBy :: Eq a => [a] -> [a] -> [[a]]
endBy = split . dropFinalBlank . onSublist

endByOneOf :: Eq a => [a] -> [a] -> [[a]]
endByOneOf = split . dropFinalBlank . oneOf

unintercalate :: Eq a => [a] -> [a] -> [[a]]
unintercalate = endBy

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

splitEvery :: Int -> [e] -> [[e]]
splitEvery i l = map (take i) (build (splitter l)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

splitPlaces :: [Int] -> [e] -> [[e]]
splitPlaces ls xs = build (splitPlacer ls xs) where
  splitPlacer [] _ _ n      = n
  splitPlacer _ [] _ n      = n
  splitPlacer (l:ls) xs c n = let (x1, x2) = splitAt l xs in x1 `c` splitPlacer ls x2 c n

splitPowersOf2 :: [e] -> [[e]]
splitPowersOf2 = splitPlaces (iterate (*2) 1)

-- | split at regular intervals
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

-- for testing, code from vixey
{-
match [] string = Just string
match (_:_) [] = Nothing
match (p:ps) (q:qs) | p == q = match ps qs
match (_:_)  (_:_)  | otherwise = Nothing

chopWith delimiter (match delimiter -> Just tail) = return ([], tail)
chopWith delimiter (c:cs) = chopWith delimiter cs >>= \(head, tail) ->
                              return (c:head, tail)
chopWith delimiter [] = Nothing
-- note: chopWith could be make 'more efficient' i.e. remove the >>=\-> bit
--       by adding an accumulator


unintercalate delimiter = unfoldr (chopWith delimiter)
-}