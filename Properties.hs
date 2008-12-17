module Properties where

import Data.List.Split.Internals
import Test.QuickCheck

import System.Environment
import Text.Printf
import Control.Monad

import Data.Char
import Data.List (isInfixOf)

newtype Elt = Elt { unElt :: Char }
  deriving (Eq)

instance Show Elt where
  show (Elt c) = show c

instance Arbitrary Elt where
  arbitrary = elements (map Elt "abcde")

instance CoArbitrary Elt where
  coarbitrary = coarbitrary . ord . unElt

instance (Arbitrary a, CoArbitrary a, Eq a) => Arbitrary (Delimiter a) where
  arbitrary = oneof [ liftM DelimEltPred arbitrary
                    , liftM DelimSublist arbitrary
                    ]

instance Arbitrary a => Arbitrary (SplitElem a) where
  arbitrary = oneof [ liftM Chunk (listOf arbitrary)
                    , liftM Delim (listOf arbitrary)
                    ]

instance Arbitrary DelimPolicy where
  arbitrary = elements [Drop, Keep, KeepLeft, KeepRight]

instance Arbitrary CondensePolicy where
  arbitrary = elements [Condense, KeepBlankFields]

instance Arbitrary EndPolicy where
  arbitrary = elements [DropBlank, KeepBlank]

instance (Arbitrary a, CoArbitrary a, Eq a) => Arbitrary (Splitter a) where
  arbitrary = liftM5 Splitter arbitrary arbitrary arbitrary arbitrary arbitrary

main :: IO ()
main = do
    results <- mapM (\(s,t) -> printf "%-40s: " s >> t) tests
    when (not . all isSuccess $ results) $ fail "Not all tests passed!"
 where
    isSuccess (Success{}) = True
    isSuccess _ = False
    qc x = quickCheckResult x
    tests = [ ("default/id",         qc prop_default_id)
            , ("match/decompose",    qc prop_match_decompose)
            , ("match/yields delim", qc prop_match_yields_delim)
            , ("splitInternal/lossless", qc prop_splitInternal_lossless)
            , ("splitInternal/yields delims", qc prop_splitInternal_yields_delims)
            , ("splitInternal/chunks", qc prop_splitInternal_chunks_not_delims)
            , ("condense/no consec delims", qc prop_condense_no_consec_delims)
            , ("insBlanks/no consec delims", qc prop_insBlanks_no_consec_delims)
            , ("insBlanks/fl not delims", qc prop_insBlanks_fl_not_delim)
            , ("mergeL/no delims", qc prop_mergeL_no_delims)
            , ("mergeR/no delims", qc prop_mergeR_no_delims)
            , ("oneOf", qc prop_oneOf)
            , ("oneOf/not chunks", qc prop_oneOf_not_chunks)
            , ("onSublist", qc prop_onSublist)
            , ("onSublist/not chunks", qc prop_onSublist_not_chunks)
            , ("whenElt", qc prop_whenElt)
            , ("whenElt/not chunks", qc prop_whenElt_not_chunks)
            ]

-- The default splitting strategy is the identity.
prop_default_id :: [Elt] -> Bool
prop_default_id l = split defaultSplitter l == [l]

prop_match_decompose :: Blind (Delimiter Elt) -> [Elt] -> Bool
prop_match_decompose (Blind d) l = maybe True ((==l) . uncurry (++)) $ matchDelim d l

isDelimMatch :: Delimiter Elt -> [Elt] -> Bool
isDelimMatch d l = matchDelim d l == Just (l,[])

prop_match_yields_delim :: Blind (Delimiter Elt) -> [Elt] -> Bool
prop_match_yields_delim (Blind d) l =
    case matchDelim d l of
      Nothing -> True
      Just (del,rest) -> isDelimMatch d del

prop_splitInternal_lossless :: Blind (Delimiter Elt) -> [Elt] -> Bool
prop_splitInternal_lossless (Blind d) l = concatMap fromElem (splitInternal d l) == l

prop_splitInternal_yields_delims :: Blind (Delimiter Elt) -> [Elt] -> Bool
prop_splitInternal_yields_delims (Blind d) l =
    all (isDelimMatch d) $ [ del | (Delim del) <- splitInternal d l ]

prop_splitInternal_chunks_not_delims :: Blind (Delimiter Elt) -> [Elt] -> Bool
prop_splitInternal_chunks_not_delims (Blind d) l =
    all (not . isDelimMatch d) $ [ ch | (Chunk ch) <- splitInternal d l ]

noConsecDelims :: SplitList Elt -> Bool
noConsecDelims [] = True
noConsecDelims [x] = True
noConsecDelims (Delim _ : Delim _ : _) = False
noConsecDelims (_ : xs) = noConsecDelims xs

prop_condense_no_consec_delims :: SplitList Elt -> Bool
prop_condense_no_consec_delims l = noConsecDelims $ doCondense Condense l

prop_insBlanks_no_consec_delims :: SplitList Elt -> Bool
prop_insBlanks_no_consec_delims l = noConsecDelims $ insertBlanks l

prop_insBlanks_fl_not_delim :: SplitList Elt -> Bool
prop_insBlanks_fl_not_delim l =
    case insertBlanks l of
      [] -> True
      xs -> (not . isDelim $ head xs) && (not . isDelim $ last xs)

prop_mergeL_no_delims :: SplitList Elt -> Bool
prop_mergeL_no_delims = all (not . isDelim) . mergeLeft . insertBlanks

prop_mergeR_no_delims :: SplitList Elt -> Bool
prop_mergeR_no_delims = all (not . isDelim) . mergeRight . insertBlanks

getDelims :: Splitter Elt -> [Elt] -> [[Elt]]
getDelims s l = [ d | Delim d <- splitInternal (delimiter s) l ]

getChunks :: Splitter Elt -> [Elt] -> [[Elt]]
getChunks s l = [ c | Chunk c <- splitInternal (delimiter s) l ]

prop_oneOf :: [Elt] -> [Elt] -> Bool
prop_oneOf elts l = all ((==1) . length) ds && all ((`elem` elts) . head) ds
  where ds = getDelims (oneOf elts) l

prop_oneOf_not_chunks :: [Elt] -> [Elt] -> Bool
prop_oneOf_not_chunks elts l = all (not . (`elem` elts)) (concat cs)
  where cs = getChunks (oneOf elts) l

prop_onSublist :: [Elt] -> [Elt] -> Bool
prop_onSublist sub l = all (==sub) $ getDelims (onSublist sub) l

prop_onSublist_not_chunks :: [Elt] -> [Elt] -> Property
prop_onSublist_not_chunks sub l =
    (not . null $ sub) ==>
      all (not . isInfixOf sub) $ getChunks (onSublist sub) l

prop_whenElt :: Blind (Elt -> Bool) -> [Elt] -> Bool
prop_whenElt (Blind p) l = all ((==1) . length) ds && all (p . head) ds
  where ds = getDelims (whenElt p) l

prop_whenElt_not_chunks :: Blind (Elt -> Bool) -> [Elt] -> Bool
prop_whenElt_not_chunks (Blind p) l = all (not . p) (concat cs)
  where cs = getChunks (whenElt p) l

{-
-- | split at regular intervals
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs
-}

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