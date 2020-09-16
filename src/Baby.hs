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

describeList :: [a] -> String
describeList ls =
  "The list is "
    ++ case ls of
      [] -> "empty."
      [x] -> "a singleton list"
      xs -> "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "error"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n -1) x

take' :: Int -> [x] -> [x]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : (take' (n -1) xs)

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (a : as) (b : bs) = (a, b) : zip' as bs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let small = [a | a <- xs, a <= x]
      big = [a | a <- xs, a > x]
   in quicksort small ++ [x] ++ big

fib :: Int -> Int
fib x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fib (x -1) + fib (x -2)

comHundred :: Int -> Ordering
comHundred x = compare 100 x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
