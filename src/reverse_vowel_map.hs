-- https://github.com/keon/algorithms/blob/master/string/reverse_vowel.py

import qualified Data.Map.Strict as Map
import Data.Maybe

-- A change from postion left to position right
data Change = Change Int Int deriving (Show)

isVowel :: Char -> Bool
isVowel c = c `elem` "AEIOUYaeiouy"


-- Search for vowel in m from position in direction (+|-) up to limit
lookupVowel :: Map.Map k Char -> Int -> (Int -> Int -> Int) -> Int -> Maybe (k, Char)
lookupVowel m p direction limit
    | isVowel $ snd c   = Just c
    | p == limit        = Nothing
    | otherwise         = lookupVowel m (direction p 1) direction limit
    where c = Map.elemAt p m

-- Find fist vowel in map
findFirstVowel :: Map.Map k Char -> Maybe (k, Char)
findFirstVowel m = lookupVowel m 0 (+) (Map.size m - 1)

-- Find last vowel in map
findLastVowel :: Map.Map k Char -> Maybe (k, Char)
findLastVowel m = lookupVowel m (Map.size m - 1) (-) 0


shortenMap :: Map.Map Int Char -> Int -> Int -> Map.Map Int Char
shortenMap m leftKey rightKey = fst $ Map.split rightKey smallL
    where (_, smallL) = Map.split leftKey m

justFst :: Maybe (Int, Char) -> Int
justFst (Just mapElem) =  fst mapElem
justFst Nothing = 0


-- Create list of changes
crawlMap :: Map.Map Int Char -> [Change] -> [Change]
crawlMap m changes
    | Map.size m == 0                   = changes
    | isNothing left || isNothing right = changes
    | leftKey >= rightKey               = changes
    | otherwise                         =
        crawlMap (shortenMap m leftKey rightKey) (Change leftKey rightKey: changes)
    where
        left = findFirstVowel m
        right = findLastVowel m
        leftKey = justFst left
        rightKey = justFst right



-- Flip to letters in mapped string
applyChange :: Change -> Map.Map Int Char -> Map.Map Int Char
applyChange (Change leftKey rightKey) m = Map.insert rightKey leftValue (Map.insert leftKey rightValue m)
    where
        leftValue = m Map.! leftKey
        rightValue = m Map.! rightKey



reverseVowel :: String -> String
reverseVowel xs = Map.elems reversed
    where
        mappedString = Map.fromList $ zip [0..] xs
        changes      = crawlMap mappedString []
        reversed     = foldr applyChange mappedString changes



-- Hello
-- 01234
-- 04231
-- Holle


main :: IO()
main = print (reverseVowel "Hello")
