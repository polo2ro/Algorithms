import Data.Maybe

-- The LetterDict data type contain on level of trie
data LetterDict = LetterDict { letter :: Char, dictlist :: [LetterDict] } deriving (Show)


-- Get the list of LetterDict associated to letter
findKey :: Char -> [LetterDict] -> Maybe [LetterDict]
findKey key [] = Nothing
findKey key (letterDict:xs) = if key == letter letterDict
                                then Just (dictlist letterDict)
                                else findKey key xs

-- Argument for drillDict
data LevelArg = LevelArg { word :: String, leveldl :: [LetterDict], position :: Int } deriving (Show)




-- Create new LevelArg for the next position in word
nextlevel :: LevelArg -> Maybe LevelArg
nextlevel level =
    if position level+1 < length (word level)
        then Just (LevelArg (word level) (leveldl level) (position level +1))
        else Nothing





-- Add letter and recursive dicts to dict
drillDict :: Maybe LevelArg -> [LetterDict]
drillDict Nothing = []
drillDict (Just level) =
    let letter = word level !! position level
        new = LetterDict letter (drillDict $ nextlevel level)
        leveldlLocal = leveldl level
        letterList = findKey letter leveldlLocal
        res = case letterList of
            Just letterList -> new : letterList
            Nothing -> LetterDict letter [new] : leveldlLocal
    in
        [new]





buildTrie :: [String] -> [LetterDict]
buildTrie words =
    let args = [Just (LevelArg w [] 0) | w <- words]
    in head (map drillDict args)



testlist = ["hello", "hey", "what", "when", "why"]



testtrie = [
        LetterDict 'a' [
            LetterDict 'p' [
                LetterDict 'p' [
                    LetterDict 'l' [
                        LetterDict 'e' []
                    ]
                ]
            ],
            LetterDict 's' [
                LetterDict 's' []
            ]
        ]
    ]

main = do
    let trie = buildTrie testlist
    --print trie
    print (drillDict (Just (LevelArg "apple" [] 0)))
    --print (findKey 'b' testtrie)
