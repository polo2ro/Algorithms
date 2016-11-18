import Debug.Trace

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

drillDict :: (String, [(Char, String)]) -> [(Char, String)]
drillDict t = do
    let wordd = fst t
    let letter = head wordd
    let dict = snd t
    let value = findKey letter dict
    if isNothing value then (letter,[wordd]):dict else wordd:value
    return dict


buildTrie :: [String] -> [(Char, String)]
buildTrie words = do
    let dict = []
    map drillDict [(w, dict) | w <- words]
    return dict

isWordInTrie word = word

testlist = ["hello", "hey", "what", "when", "why"]

main = do
    let trie = buildTrie testlist
    print trie
