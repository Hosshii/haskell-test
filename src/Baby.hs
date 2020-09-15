module Baby
  ( doubleMe,
    initials',
    calcBmis,
  )
where

doubleMe x = x * 2

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "Its a-me, Conan O'Brien!"

-- removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

-- removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- factorial :: Integer -> Integer
-- factorial n = product [1 .. n]

lucky :: Int -> String
lucky 7 = "SEVEN"
lucky x = "NOT SEVEN"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"

head' :: [a] -> a
head' [] = error "Cannot use empty list"
head' (x : _) = x

firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

calcBmi :: Double -> Double -> Double
calcBmi weight height = weight / height ^ 2

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "yaseteru"
  | bmi <= 25 = "hutuu"
  | bmi <= 30 = "hutotteru"
  | otherwise = "kuzira"

bmiCalcTell :: Double -> Double -> String
bmiCalcTell weight height
  | calcBmi weight height <= 18.5 = "yaseteru"
  | calcBmi weight height <= 25.0 = "hutuu"
  | calcBmi weight height <= 30.0 = "himan"
  | otherwise = "kuzira"

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

initials' :: String -> String -> String
initials' firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= skinny = "yase"
  | bmi <= normal = "hutuujjjjjjjjjjj"
  | bmi <= fat = "himan"
  | otherwise = "kuzira"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 38.0

bmiAge a b = a + b

calcBmis :: [(Double, Double)] -> [Double]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--   where
--     bmi w h = w / h ^ 2
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi <= 18.5]