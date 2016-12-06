-- http://stackoverflow.com/questions/24414700/amazon-water-collected-between-towers
-- Input: [1,5,3,7,2] , Output: 2 units
-- Input: [5,3,7,2,6,4,5,9,1,2] , Output: 14 units

rainfall :: [Int] -> Int
rainfall xs = sum (filter (>0) (zipWith (-) deltas xs))
    where deltas = zipWith min (init maxl) (tail maxr)
          maxr   = scanr max 0 xs
          maxl   = scanl max 0 xs

main :: IO()
main = print (rainfall [1,5,3,7,2])
