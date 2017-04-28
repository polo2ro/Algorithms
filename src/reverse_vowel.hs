-- https://github.com/keon/algorithms/blob/master/string/reverse_vowel.py


getVoyPos :: (Char, Int) -> (Char, Int)
getVoyPos letterPos
    | any (\x -> fst letterPos == x) vowels  = letterPos
    | otherwise                              = (fst letterPos, -1)
    where vowels = "AEIOUYaeiouy"


havePos :: (Char, Int) -> Bool
havePos (_, p) = p >= 0


changePos :: (Char, Int) -> (Char, Int) -> (Char, Int)
changePos (c, _) (_, p) = (c, p)


applyChange :: (Char, Int) -> String -> String
applyChange (c, p) str = begin ++ [c] ++ tail end
    where
        (begin, end) = splitAt p str





reverseVowel :: String -> String
reverseVowel xs = foldr applyChange xs changes
    where
        limit   = take $ quot (length xs) 2
        vpos    = zipWith (curry getVoyPos) xs [0..]
        begin   = filter havePos $ limit vpos
        end     = filter havePos $ limit (reverse vpos)
        ml      = min (length begin) (length end)
        b       = take ml begin
        e       = take ml end
        changes = zipWith changePos b e ++ zipWith changePos e b


-- Hello
-- 01234
-- 04231
-- Holle


main :: IO()
main = print (reverseVowel "Hello")
