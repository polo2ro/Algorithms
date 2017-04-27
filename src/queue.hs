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



-- Filter list, get the n first found persons with height >= h
-- The number of persons in the returned list will be filtered using the 2nd parameter function
filterHeight :: Int -> (Int -> Bool) -> [(Int, Int)] -> [(Int, Int)]
filterHeight _ _ [] = []
filterHeight h p (x:xs)
    | p (length [person | person <- xs, fst person <= h]) = x : filterHeight h p xs
    | otherwise = filterHeight h p xs


reconstruct :: [(Int, Int)] -> [(Int, Int)]
reconstruct [] = []
reconstruct (x:xs) =
        let
        h = fst x
        k = snd x
        before = reconstruct (filterHeight h (<=k) xs)
        after  = reconstruct (filterHeight h (>k) xs)
    in  before ++ [x] ++ after




main :: IO()
main = print (reconstruct [(7,0), (4,4), (7,1), (5,0), (6,1), (5,2)])
