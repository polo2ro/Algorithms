import Debug.Trace


-- The LetterDict data type contain on level of trie
-- LetterDict a (LetterDict b (LetterDict c Nothing))
data LetterDict = LetterDict { letter :: Char, dictlist :: [LetterDict] } deriving (Show)


-- Get the list of LetterDict associated to letter
findKey :: (Eq k) => k -> [LetterDict] -> Maybe [LetterDict]
findKey key [] = Nothing
findKey key xs = foldr (\letterDict
                    -> if key == letter letterDict then
                        Just (dictlist letterDict)
                    else
                        letterDict)
                    Nothing xs



{-|

-- Argument for drillDict
data LevelArg = LevelArg { word :: String, dict :: [Maybe LetterDict], position :: Int } deriving (Show)


-- Create new LevelArg for the next position in word
nextlevel :: LevelArg -> LevelArg
nextlevel level =
    LevelArg (word level) (dict level) (position level +1)

-- Add letter and recursive dicts to dict
drillDict :: LevelArg -> [LetterDict]
drillDict level =
    let letter = word level !! position level
        letterDictList = findKey letter $ dict level
    in
        if isNothing letterDictList
            then LetterDict letter (drillDict $ nextlevel level):dict level
            else word:letterDictList
    letterDictList


buildTrie :: [String] -> [(Char, String)]
buildTrie words =
    let dict = [] -- This is a list of LetterDict
    in map drillDict [LevelArg w dict 0 | w <- words]

isWordInTrie word = word

testlist = ["hello", "hey", "what", "when", "why"]

-}

testtrie = [
        LetterDict 'a' [
            LetterDict 'p' [
                LetterDict 'p' [
                    LetterDict 'l' [
                        LetterDict 'e' []
                    ]
                ]
            ]
        ]
    ]

main = do
    --let trie = buildTrie testlist
    --print trie
    print testtrie
    print (findKey 'a' testtrie)
