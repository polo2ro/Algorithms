-- Given a List of words, return the words that can be typed using letters of
-- alphabet on only one row's of American keyboard.
-- https://github.com/keon/algorithms/blob/master/algorithms/set/find_keyboard_row.py
-- https://leetcode.com/problems/keyboard-row/description/

import Data.List
import Data.Maybe
import Data.Char

checkRow :: String -> String -> Bool
checkRow word row = search == search `intersect` row
    where search = map toLower word

isInOneKeyboardRow :: String -> Bool
isInOneKeyboardRow word = isJust $ find (checkRow word)
    [
        "qwertyuiop",
        "asdfghjkl",
        "zxcvbnm"
    ]

main :: IO()
main = print (filter isInOneKeyboardRow ["Hello", "Alaska", "Dad", "Peace"])
