module Properties where

import Data.List.Split.Internals
import Test.QuickCheck

import System.Environment
import Text.Printf
import Control.Monad

import Data.Char

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