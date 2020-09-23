module Optional where

import Data.Char

-- findKey :: (Eq k) => k -> [(k, v)] -> v
-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v) : xs)
--   | key == k = Just v
--   | otherwise = findKey key xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit