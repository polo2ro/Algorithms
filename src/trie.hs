
generateTrieFromWords words = do
    let root = ()
    let wordx = head words
    let wordy = drop 1 wordx
    wordy

testlist = ["hello", "hey", "what", "when", "why"]

main = do
    let trie = generateTrieFromWords testlist
    print trie
