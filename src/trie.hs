import Data.Maybe

-- The LetterDict data type contain on level of trie
-- LetterDict a (LetterDict b (LetterDict c Nothing))
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
nextlevel :: LevelArg -> LevelArg
nextlevel level =
    LevelArg (word level) (leveldl level) (position level +1)





-- Add letter and recursive dicts to dict
drillDict :: LevelArg -> [LetterDict]
drillDict level =
    let letter = word level !! position level
        leveldlLocal = leveldl level
        letterList = findKey letter leveldlLocal
        new = LetterDict letter leveldlLocal
        nextList = drillDict $ nextlevel level
    in
        case letterList of
            Just letterList -> new : letterList
            Nothing -> LetterDict letter [new] : leveldlLocal







buildTrie :: [String] -> [LetterDict]
buildTrie words =
    let dict = [] -- This is a list of LetterDict
        args = [LevelArg w dict 0 | w <- words]
    in
        map drillDict args



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
    print trie
    --print (drillDict (LevelArg "apple" [] 0))
    --print (findKey 'a' testtrie)
