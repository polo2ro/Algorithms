import Debug.Trace

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

drillDict :: (String, [(Char, String)]) -> Maybe String
drillDict t = do
    let wordd = fst t
    let letter = head wordd
    let dict = snd t
    trace ("wordd: " ++ show wordd ++ ", letter: " ++ show letter) (findKey letter dict)



buildTrie :: [String] -> [Maybe String]
buildTrie words = do
    let dict = []
    map drillDict [(w, dict) | w <- words]

isWordInTrie word = word

testlist = ["hello", "hey", "what", "when", "why"]

main = do
    let trie = buildTrie testlist
    print trie
