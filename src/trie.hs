import Debug.Trace

-- Get the list of LetterDict associated to letter
findKey :: (Eq k) => k -> LetterDict -> Maybe [LetterDict]
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- The LetterDict data type contain on level of trie
-- dict 1 ex: ("a", [])
-- dict 1 and dict2 ex: ("a", [("b", [])])
data LetterDict = LetterDict Char [LetterDict] deriving (Show)

-- Argument for drillDict
data LevelArg = LevelArg { word :: String, dict :: [LetterDict], position :: Int } deriving (Show)


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
    in map drillDict [(w, dict, 0) | w <- words]

isWordInTrie word = word

testlist = ["hello", "hey", "what", "when", "why"]

main = do
    let trie = buildTrie testlist
    print trie
