{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Properties where

import           Data.List.Split.Internals
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Control.Monad
import           Distribution.TestSuite
import           System.Environment
import           Text.Printf

import           Data.Char
import           Data.Functor
import           Data.List                 (genericTake, group, intercalate, isInfixOf, isPrefixOf,
 isSuffixOf, tails)
import           Data.Maybe                (catMaybes, fromJust, isJust, maybe)
import           Data.Typeable             (Typeable(..))
import           System.Random             (StdGen, newStdGen, next)

import qualified Distribution.TestSuite    as Cabal

data QCTest = forall prop. Testable prop => QCTest String prop

test :: Testable prop => String -> prop -> Cabal.Test
test n p = Cabal.impure $ QCTest n p

instance Cabal.TestOptions QCTest where
    name (QCTest n _) = n

    options _ =
        [ ("std-gen", typeOf (undefined :: String))
        , ("max-success", typeOf (undefined :: Int))
        , ("max-discard", typeOf (undefined :: Int))
        , ("size", typeOf (undefined :: Int))
        ]

    defaultOptions _ = do
        rng <- newStdGen
        return $ Cabal.Options $
            [ ("std-gen", show rng)
            , ("max-success", show $ maxSuccess stdArgs)
            , ("max-discard", show $ maxDiscard stdArgs)
            , ("size", show $ maxSize stdArgs)
            ]

    check t (Cabal.Options opts) = catMaybes
        [ maybeNothing "max-success" ([] :: [(Int, String)])
        , maybeNothing "max-discard" ([] :: [(Int, String)])
        , maybeNothing "size" ([] :: [(Int, String)])
        ]
        -- There is no need to check the parsability of "std-gen"
        -- because the Read instance for StdGen always succeeds.
        where
            maybeNothing n x =
                maybe Nothing (\str ->
                    if reads str == x then Just n else Nothing)
                    $ lookup n opts

instance Cabal.ImpureTestable QCTest where
    runM (QCTest _ prop) o =
        catch go (return . Cabal.Error . show)
        where
            go = do
                result <- quickCheckWithResult args prop
                return $ case result of
                        Success {} -> Cabal.Pass
                        GaveUp {}->
                            Cabal.Fail $ "gave up after "
                                       ++ show (numTests result)
                                       ++ " tests"
                        Failure {} -> Cabal.Fail $ reason result
                        NoExpectedFailure {} ->
                            Cabal.Fail "passed (expected failure)"
            args = Args
                { replay = Just
                    ( Cabal.lookupOption "std-gen" o
                    , Cabal.lookupOption "size" o
                    )
                , maxSuccess = Cabal.lookupOption "max-success" o
                , maxDiscard = Cabal.lookupOption "max-discard" o
                , maxSize = Cabal.lookupOption "size" o
                }

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype Elt = Elt { unElt :: Char }
  deriving (Eq)

instance Show Elt where
  show (Elt c) = show c

instance Arbitrary Elt where
  arbitrary = elements (map Elt "abcde")

instance CoArbitrary Elt where
  coarbitrary = coarbitrary . ord . unElt

instance Function Elt where
  function = functionMap unElt Elt

deriving instance Show (Splitter Elt)

instance Show (Delimiter Elt) where
  show (Delimiter ps) = show (map function ps)

instance (Arbitrary a, CoArbitrary a, Function a) => Arbitrary (Delimiter a) where
  arbitrary = (Delimiter . map apply) <$> arbitrary

instance Arbitrary a => Arbitrary (Chunk a) where
  arbitrary = oneof [ liftM Text (listOf arbitrary)
                    , liftM Delim (listOf arbitrary)
                    ]

instance Arbitrary DelimPolicy where
  arbitrary = elements [Drop, Keep, KeepLeft, KeepRight]

instance Arbitrary CondensePolicy where
  arbitrary = elements [Condense, KeepBlankFields]

instance Arbitrary EndPolicy where
  arbitrary = elements [DropBlank, KeepBlank]

instance (Arbitrary a, CoArbitrary a, Function a) => Arbitrary (Splitter a) where
  arbitrary = liftM5 Splitter arbitrary arbitrary arbitrary arbitrary arbitrary

type Delim a = [Fun a Bool]

unDelim :: Delim a -> Delimiter a
unDelim = Delimiter . map apply

tests :: [Test]
tests = [ test "default/id"                    prop_default_id
        , test "match/decompose"               prop_match_decompose
        , test "match/yields delim"            prop_match_yields_delim
        , test "splitInternal/lossless"        prop_splitInternal_lossless
        , test "splitInternal/yields delims"   prop_splitInternal_yields_delims
        , test "splitInternal/text"            prop_splitInternal_text_not_delims
        , test "doCondense/no consec delims"   prop_doCondense_no_consec_delims
        , test "insBlanks/no consec delims"    prop_insBlanks_no_consec_delims
        , test "insBlanks/fl not delims"       prop_insBlanks_fl_not_delim
        , test "mergeL/no delims"              prop_mergeL_no_delims
        , test "mergeR/no delims"              prop_mergeR_no_delims
        , test "oneOf"                         prop_oneOf
        , test "oneOf/not text"                prop_oneOf_not_text
        , test "onSublist"                     prop_onSublist
        , test "onSublist/not text"            prop_onSublist_not_text
        , test "whenElt"                       prop_whenElt
        , test "whenElt/not text"              prop_whenElt_not_text
        , test "process/dropDelims"            prop_dropDelims
        , test "process/keepDelimsL no delims" prop_keepDelimsL_no_delims
        , test "process/keepDelimsR no delims" prop_keepDelimsR_no_delims
        , test "process/keepDelimsL match"     prop_keepDelimsL_match
        , test "process/keepDelimsR match"     prop_keepDelimsR_match
        , test "condense/no consec delims"     prop_condense_no_consec_delims
        , test "condense/all delims"           prop_condense_all_delims
        , test "dropInitBlank"                 prop_dropInitBlank
        , test "dropFinalBlank"                prop_dropFinalBlank
        , test "dropBlanks"                    prop_dropBlanks
        , test "startsWith"                    prop_startsWith
        , test "startsWithOneOf"               prop_startsWithOneOf
        , test "endsWith"                      prop_endsWith
        , test "endsWithOneOf"                 prop_endsWithOneOf
        , test "splitOn/right inv"             prop_splitOn_right_inv
        , test "splitOn/idem"                  prop_splitOn_intercalate_idem
        , test "splitOn/empty delim"           prop_splitOn_empty_delim
        , test "split/empty delim"             prop_split_empty_delim_drop
        , test "splitEvery/lengths"            prop_splitEvery_all_n
        , test "splitEvery/last <= n"          prop_splitEvery_last_less_n
        , test "splitEvery/preserve"           prop_splitEvery_preserve
        , test "splitPlaces/lengths"           prop_splitPlaces_lengths
        , test "splitPlaces/last <= n"         prop_splitPlaces_last_less_n
        , test "splitPlaces/preserve"          prop_splitPlaces_preserve
        , test "splitPlaces/splitEvery"        prop_splitPlaces_splitEvery
        , test "splitPlacesB/length"           prop_splitPlacesB_length
        , test "splitPlacesB/last <= n"        prop_splitPlacesB_last_less_n
        , test "splitPlacesB/preserve"         prop_splitPlacesB_preserve
        , test "lines"                         prop_lines
        , test "wordsBy/words"                 prop_wordsBy_words
        , test "linesBy/lines"                 prop_linesBy_lines
        , test "chop/group"                    prop_chop_group
        , test "chop/words"                    prop_chop_words
        ]

prop_default_id :: [Elt] -> Bool
prop_default_id l = split defaultSplitter l == [l]

prop_match_decompose :: Delim Elt -> [Elt] -> Bool
prop_match_decompose d l = maybe True ((==l) . uncurry (++)) $ matchDelim (unDelim d) l

isDelimMatch :: Delim Elt -> [Elt] -> Bool
isDelimMatch d l = matchDelim (unDelim d) l == Just (l,[])

prop_match_yields_delim :: Delim Elt -> [Elt] -> Bool
prop_match_yields_delim d l =
    case matchDelim (unDelim d) l of
      Nothing -> True
      Just (del,rest) -> isDelimMatch d del

prop_splitInternal_lossless :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_lossless d l = concatMap fromElem (splitInternal (unDelim d) l) == l

prop_splitInternal_yields_delims :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_yields_delims d l =
    all (isDelimMatch d) $ [ del | (Delim del) <- splitInternal d' l ]
  where d' = unDelim d

prop_splitInternal_text_not_delims :: Delim Elt -> [Elt] -> Bool
prop_splitInternal_text_not_delims d l =
    all (not . isDelimMatch d) $ [ ch | (Text ch) <- splitInternal d' l ]
  where d' = unDelim d

noConsecDelims :: SplitList Elt -> Bool
noConsecDelims [] = True
noConsecDelims [x] = True
noConsecDelims (Delim _ : Delim _ : _) = False
noConsecDelims (_ : xs) = noConsecDelims xs

prop_doCondense_no_consec_delims :: SplitList Elt -> Bool
prop_doCondense_no_consec_delims l = noConsecDelims $ doCondense Condense l

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

getTexts :: Splitter Elt -> [Elt] -> [[Elt]]
getTexts s l = [ c | Text c <- splitInternal (delimiter s) l ]

prop_oneOf :: [Elt] -> [Elt] -> Bool
prop_oneOf elts l = all ((==1) . length) ds && all ((`elem` elts) . head) ds
  where ds = getDelims (oneOf elts) l

prop_oneOf_not_text :: [Elt] -> [Elt] -> Bool
prop_oneOf_not_text elts l = all (not . (`elem` elts)) (concat cs)
  where cs = getTexts (oneOf elts) l

prop_onSublist :: [Elt] -> [Elt] -> Bool
prop_onSublist sub l = all (==sub) $ getDelims (onSublist sub) l

prop_onSublist_not_text :: [Elt] -> [Elt] -> Property
prop_onSublist_not_text sub l =
    (not . null $ sub) ==>
      all (not . isInfixOf sub) $ getTexts (onSublist sub) l

prop_whenElt :: (Fun Elt Bool) -> [Elt] -> Bool
prop_whenElt (Fun _ p) l = all ((==1) . length) ds && all (p . head) ds
  where ds = getDelims (whenElt p) l

prop_whenElt_not_text :: (Fun Elt Bool) -> [Elt] -> Bool
prop_whenElt_not_text (Fun _ p) l = all (not . p) (concat cs)
  where cs = getTexts (whenElt p) l

process :: Splitter Elt -> [Elt] -> SplitList Elt
process s = postProcess s . splitInternal (delimiter s)

prop_dropDelims :: Splitter Elt -> [Elt] -> Bool
prop_dropDelims s l = all (not . isDelim) (process (dropDelims s) l)

prop_keepDelimsL_no_delims :: Splitter Elt -> [Elt] -> Bool
prop_keepDelimsL_no_delims s l = all (not . isDelim) (process (keepDelimsL s) l)

prop_keepDelimsL_match :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_keepDelimsL_match s (NonEmpty l) =
  all (isJust . matchDelim (delimiter s)) [ c | Text c <- tail p ]
    where p = process (keepDelimsL s) l

prop_keepDelimsR_no_delims :: Splitter Elt -> [Elt] -> Bool
prop_keepDelimsR_no_delims s l = all (not . isDelim) (process (keepDelimsR s) l)

prop_keepDelimsR_match :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_keepDelimsR_match s (NonEmpty l) =
  all (any (isJust . matchDelim (delimiter s)) . tails)
    [ c | Text c <- init p ]
      where p = process (keepDelimsR s) l

prop_condense_no_consec_delims :: Splitter Elt -> [Elt] -> Bool
prop_condense_no_consec_delims s l = noConsecDelims $ process (condense s) l

prop_condense_all_delims :: Splitter Elt -> [Elt] -> Bool
prop_condense_all_delims s l = all allDelims p
  where p = [ d | Delim d <- process (condense s) l ]
        allDelims t = all isDelim (splitInternal (delimiter s) t)

prop_dropInitBlank :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_dropInitBlank s (NonEmpty l) = head p /= Text []
  where p = process (dropInitBlank $ s { delimPolicy = Keep } ) l

prop_dropFinalBlank :: Splitter Elt -> NonEmptyList Elt -> Bool
prop_dropFinalBlank s (NonEmpty l) = last p /= Text []
  where p = process (dropFinalBlank $ s { delimPolicy = Keep } ) l

prop_dropBlanks :: Splitter Elt -> [Elt] -> Bool
prop_dropBlanks s = null . filter (== (Text [])) . process (dropBlanks s)

prop_startsWith :: [Elt] -> NonEmptyList Elt -> Bool
prop_startsWith s (NonEmpty l) = all (s `isPrefixOf`) (tail $ split (startsWith s) l)

prop_startsWithOneOf :: [Elt] -> NonEmptyList Elt -> Bool
prop_startsWithOneOf elts (NonEmpty l) = all ((`elem` elts) . head) (tail $ split (startsWithOneOf elts) l)

prop_endsWith :: [Elt] -> NonEmptyList Elt -> Bool
prop_endsWith s (NonEmpty l) = all (s `isSuffixOf`) (init $ split (endsWith s) l)

prop_endsWithOneOf :: [Elt] -> NonEmptyList Elt -> Bool
prop_endsWithOneOf elts (NonEmpty l) = all ((`elem` elts) . last) (init $ split (endsWithOneOf elts) l)

prop_splitOn_right_inv :: [Elt] -> [Elt] -> Bool
prop_splitOn_right_inv x l = intercalate x (splitOn x l) == l

{- This property fails: for example,

      splitOn "dd" (intercalate "dd" ["d",""]) == ["","d"]

   so it's not enough just to say that the delimiter is not an infix of
   any elements of l!


prop_splitOn_left_inv :: [Elt] -> NonEmptyList [Elt] -> Property
prop_splitOn_left_inv x (NonEmpty ls) = not (any (x `isInfixOf`) ls) ==>
                                        splitOn x (intercalate x ls) == ls
-}

-- Note, the below property is in fact logically entailed by
-- prop_splitOn_right_inv, but we keep it here just for kicks.
prop_splitOn_intercalate_idem :: [Elt] -> [[Elt]] -> Bool
prop_splitOn_intercalate_idem x ls = f (f ls) == f ls
  where f = splitOn x . intercalate x

prop_splitOn_empty_delim :: [Elt] -> Bool
prop_splitOn_empty_delim ls = splitOn [] ls == [] : map (:[]) ls

prop_split_empty_delim_drop :: [Elt] -> Bool
prop_split_empty_delim_drop ls
  = split (dropDelims . dropBlanks $ onSublist []) ls == map (:[]) ls

prop_splitEvery_all_n :: Positive Int -> NonEmptyList Elt -> Bool
prop_splitEvery_all_n (Positive n) (NonEmpty l) = all ((==n) . length) (init $ splitEvery n l)

prop_splitEvery_last_less_n :: Positive Int -> NonEmptyList Elt -> Bool
prop_splitEvery_last_less_n (Positive n) (NonEmpty l) = (<=n) . length . last $ splitEvery n l

prop_splitEvery_preserve :: Positive Int -> [Elt] -> Bool
prop_splitEvery_preserve (Positive n) l = concat (splitEvery n l) == l

prop_splitPlaces_lengths :: [NonNegative Int] -> [Elt] -> Bool
prop_splitPlaces_lengths ps = and . mInit . zipWith (==) ps' . map length . splitPlaces ps'
  where ps' = map unNN ps

prop_splitPlaces_last_less_n :: NonEmptyList (NonNegative Int) -> NonEmptyList Elt -> Bool
prop_splitPlaces_last_less_n (NonEmpty ps) (NonEmpty l) = (head $ drop (length l' - 1) ps') >= length (last l')
  where l' = splitPlaces ps' l
        ps' = map unNN ps

prop_splitPlaces_preserve :: [NonNegative Integer] -> [Elt] -> Bool
prop_splitPlaces_preserve ps l = concat (splitPlaces ps' l) == genericTake (sum ps') l
  where ps' = map unNN ps

prop_splitPlaces_splitEvery :: Positive Int -> [Elt] -> Bool
prop_splitPlaces_splitEvery (Positive n) l = splitPlaces (repeat n) l == splitEvery n l

prop_splitPlacesB_length :: [NonNegative Int] -> [Elt] -> Bool
prop_splitPlacesB_length ps xs = length ps' == length (splitPlacesBlanks ps' xs)
  where ps' = map unNN ps

prop_splitPlacesB_last_less_n :: NonEmptyList (NonNegative Int) -> NonEmptyList Elt -> Bool
prop_splitPlacesB_last_less_n (NonEmpty ps) (NonEmpty l) = (head $ drop (length l' - 1) ps') >= length (last l')
  where l' = splitPlacesBlanks ps' l
        ps' = map unNN ps

prop_splitPlacesB_preserve :: [NonNegative Integer] -> [Elt] -> Bool
prop_splitPlacesB_preserve ps l = concat (splitPlacesBlanks ps' l) == genericTake (sum ps') l
  where ps' = map unNN ps

unNN :: NonNegative a -> a
unNN (NonNegative x) = x

mInit :: [a] -> [a]
mInit [] = []
mInit [x] = []
mInit (x:xs) = x : init xs

newtype EltWS = EltWS { unEltWS :: Char }
  deriving (Eq, Show)

instance Arbitrary EltWS where
  arbitrary = elements (map EltWS "abcde \n")

prop_lines :: [EltWS] -> Bool
prop_lines s = lines s' == endBy "\n" s'
  where s' = map unEltWS s

prop_wordsBy_words :: [EltWS] -> Bool
prop_wordsBy_words s = words s' == wordsBy isSpace s'
  where s' = map unEltWS s

prop_linesBy_lines :: [EltWS] -> Bool
prop_linesBy_lines s = lines s' == linesBy (=='\n') s'
  where s' = map unEltWS s

prop_chop_group :: [Elt] -> Bool
prop_chop_group s = chop (\xs@(x:_) -> span (==x) xs) s == group s

prop_chop_words :: [EltWS] -> Bool
prop_chop_words s = words s' == (filter (not . null) . chop (span (not . isSpace) . dropWhile isSpace) $ s')
  where s' = map unEltWS s
