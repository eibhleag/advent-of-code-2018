import qualified Data.Set as Set

parseLine :: String -> Int
parseLine (l:ls)
    | l == '-' = read (l:ls)
    | otherwise = read ls

-- Part 1
total :: [String] -> Int
total = (foldl (+) 0) . (map parseLine)

-- Part 2
totals :: Int -> [Int] -> [Int]
totals acc (l:[]) = [acc, acc + l]
totals acc (l:ls) = (:) acc $ totals (acc + l) ls

dupe :: (Set.Set Int) -> [Int] -> Int
dupe seen (x:xs)
    | Set.member x seen = x
    | otherwise = dupe (Set.insert x seen) xs

-- Lazy Lists are magic!
main = do
    interact (show . (dupe Set.empty) . (totals 0) . (map parseLine) . cycle . lines)
    putStr "\n"