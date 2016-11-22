import Data.Maybe
import Data.Foldable
import Control.Monad.Trans.RWS.Lazy

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
drillDict :: Maybe LevelArg -> [LetterDict]         -- ab, [LetterDict {letter = 'a', dictlist = [LetterDict {letter = 'c', dictlist = []}]}], 0
drillDict Nothing = []
drillDict (Just level) =
    let ll = word level !! position level           -- a
        newlist = drillDict $ nextlevel level       -- [LetterDict {letter = 'b', dictlist = []}]
        leveldlLocal = leveldl level                -- [LetterDict {letter = 'a', dictlist = [LetterDict {letter = 'b', dictlist = []}]}]
        letterList = findKey ll leveldlLocal        -- [LetterDict {letter = 'c', dictlist = []}]
        res = case letterList of
            Just letterList -> newlist ++ letterList  -- [LetterDict {letter = 'b', dictlist = []}, LetterDict {letter = 'c', dictlist = []}]
            Nothing -> newlist ++ leveldlLocal
    in
        [LetterDict ll res]





buildTrie :: [String] -> [LetterDict]
buildTrie =
    foldr (\w dict -> drillDict (Just (LevelArg w dict 0)) ++ dict) []



-- testlist = ["hello", "hey", "what", "when", "why"]
testlist = ["when", "why"]


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

main =
    print (buildTrie testlist)
    --print (drillDict (Just (LevelArg "ac" [] 0)))
    --print (drillDict (Just (LevelArg "ab" (drillDict (Just (LevelArg "ac" [] 0))) 0)))
