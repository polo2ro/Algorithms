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
findFirstVowel m  = lookupVowel m 0 (+) (quot (Map.size m) 2)

-- Find last vowel in map
findLastVowel :: Map.Map k Char -> Maybe (k, Char)
findLastVowel m  = lookupVowel m start (-) (quot start 2)
                where start = Map.size m - 1

-- Split map at at a key, if no key return map twice
splitIfPossible :: Map.Map Int Char -> Maybe (Int, Char) -> (Map.Map Int Char, Map.Map Int Char)
splitIfPossible m Nothing        = (m, m)
splitIfPossible m (Just (k, _))  = Map.split k m


createChange :: Maybe (Int, Char) -> Maybe (Int, Char) -> Change
createChange left right
    | isNothing left || isNothing right = Change 0 0
    | otherwise = Change (fst $ fromJust left) (fst $ fromJust right)


crawlMap :: Map.Map Int Char -> [Change] -> [Change]
crawlMap m changes
    | isNothing left || isNothing right = changes
    | otherwise                         = crawlMap smallR (createChange left right: changes)
    where
        left = findFirstVowel m
        right = findLastVowel m
        (_, smallL) = splitIfPossible m left
        (smallR, _) = splitIfPossible smallL right



reverseVowel :: String -> String
reverseVowel xs = show changes
    where
        mxs        = Map.fromList $ zip [0..] xs
        changes    = crawlMap mxs []



-- Hello
-- 01234
-- 04231
-- Holle


main :: IO()
main = print (reverseVowel "Hello")
