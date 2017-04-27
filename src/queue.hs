-- https://github.com/keon/algorithms/blob/master/queue/reconstruct_queue.py

-- Suppose you have a random list of people standing in a queue.
-- Each person is described by a pair of integers (h, k),
-- where h is the height of the person and k is the number of people
-- in front of this person who have a height greater than or equal to h.
-- Write an algorithm to reconstruct the queue.

-- Note:
-- The number of people is less than 1,100.

-- Example

-- Input:
-- [(7,0), (4,4), (7,1), (5,0), (6,1), (5,2)]

-- Output:
-- [(5,0), (7,0), (5,2), (6,1), (4,4), (7,1)]


import Data.List


insertAt :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertAt z y xs = as ++ (y:bs)
    where (as,bs) = splitAt z xs


buildNew :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
buildNew [] _         = []
buildNew (x:xs) queue = insertAt (snd x) x (buildNew xs queue)

sortPerson :: (Int, Int) -> (Int, Int) -> Ordering
sortPerson (h1, k1) (h2, k2)
    | -1*h1 < -1*h2 = GT
    | -1*h1 > -1*h2 = LT
    | -1*h1 == -1*h2 = compare k1 k2


reconstruct :: [(Int, Int)] -> [(Int, Int)]
reconstruct xs =
    buildNew sorted []
    where sorted = sortBy sortPerson xs




main :: IO()
main = print (reconstruct [(7,0), (4,4), (7,1), (5,0), (6,1), (5,2)])
