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
        letterDictList = findKey letter leveldlLocal
        insert = LetterDict letter (drillDict $ nextlevel level)
    in
        if Nothing == letterDictList
            then insert : leveldl level     -- insert a LetterDict in the list of this level
            else letterDictList             -- insert a LetterDict for the found letter

{-|





buildTrie :: [String] -> [(Char, String)]
buildTrie words =
    let dict = [] -- This is a list of LetterDict
    in map drillDict [LevelArg w dict 0 | w <- words]

isWordInTrie word = word

-}

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
    --let trie = buildTrie testlist
    --print trie
    print testtrie
    print (findKey 'a' testtrie)
