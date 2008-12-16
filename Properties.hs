module Properties where

import Data.List.Split.Internals
import Test.QuickCheck

import System.Environment
import Text.Printf
import Control.Monad

newtype Elt = Elt Char
  deriving (Eq)

instance Show Elt where
  show (Elt c) = show c

instance Arbitrary Elt where
  arbitrary = elements (map Elt "abc")

instance (Arbitrary a) => Arbitrary (Delimiter a) where
  arbitrary = oneOf [ liftM DelimEltPred arbitrary
                    , liftM DelimSublist arbitrary
                    ]

main :: IO ()
main = do
    results <- mapM (\(s,t) -> printf "%-40s: " s >> t) tests
    when (not . all isSuccess $ results) $ fail "Not all tests passed!"
 where
    isSuccess (Success{}) = True
    isSuccess _ = False
    qc = quickCheckResult
    tests = [("default/id" , qc prop_default_id)]

-- The default splitting strategy is the identity.
prop_default_id :: [Elt] -> Bool
prop_default_id l = split defaultSplitter l == [l]

-- prop_match_decompose ::

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