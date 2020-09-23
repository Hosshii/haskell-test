module IoTest where

import Control.Monad
import Data.Char

ioTestFunc :: IO ()
ioTestFunc = do
  putStrLn "Hello, what is you First Name?"
  firstName <- getLine
  putStrLn "Last Name"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "you name is " ++ bigFirstName ++ bigLastName

reverseWord :: IO ()
reverseWord = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      reverseWord

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

whenTest = do
  input <- getLine
  when (input == "SAO") $ do
    putStr' input