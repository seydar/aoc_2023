import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.Maybe

getMap :: IO [String]
getMap = do
  inp <- getContents
  return $ lines inp

digits = map (:[]) ['1'..'9']
digitsAndWords = digits ++ ["one",
                            "two",
                            "three",
                            "four",
                            "five",
                            "six",
                            "seven",
                            "eight",
                            "nine"]

partOne :: [String] -> Int
partOne = fromMaybe 0 . fmap sum . traverse (intsFrom . firstAndLast digits)

partTwo :: [String] -> Int
partTwo = fromMaybe 0 . fmap sum . traverse (intsFrom . firstAndLast digitsAndWords)

intsFrom :: [Maybe String] -> Maybe Int
intsFrom = liftM read . sequence . map convert

convert :: Maybe String -> Maybe Char
convert Nothing           = Nothing
convert (Just s) | elem s digits = Just (s !! 0)
                 | s == "one"    = Just '1'
                 | s == "two"    = Just '2'
                 | s == "three"  = Just '3'
                 | s == "four"   = Just '4'
                 | s == "five"   = Just '5'
                 | s == "six"    = Just '6'
                 | s == "seven"  = Just '7'
                 | s == "eight"  = Just '8'
                 | s == "nine"   = Just '9'

firstAndLast :: [String] -> String -> [Maybe String]
firstAndLast check str = [firstDigit str check, lastDigit str check]

firstDigit :: String -> [String] -> Maybe String
firstDigit str []     = Nothing
firstDigit str (i:is) = Just $ first i (firstDigit str is)
  where
    first a Nothing  = a
    first a (Just b) = lowest (a, (substringIndex a str)) (b, (substringIndex b str))


lastDigit :: String -> [String] -> Maybe String
lastDigit str []     = Nothing
lastDigit str (i:is) = Just $ last i (lastDigit str is)
  where
    rts = reverse str

    last a Nothing  = a
    last a (Just b) = lowest (a, (substringIndex (reverse a) rts)) (b, (substringIndex (reverse b) rts))

lowest (_, Nothing) (b, j) = b
lowest (a, i) (b, Nothing) = a
lowest (a, i) (b, j)       | i < j     = a
                           | otherwise = b

substringIndex :: String -> String -> Maybe Int
substringIndex sub str = substringIndex' sub str 0
  where
    substringIndex' sub ""  i = Nothing 
    substringIndex' sub str i | isPrefixOf sub str = Just i
                              | otherwise          = substringIndex' sub (tail str) (i + 1)

main = do
  args  <- getArgs
  parts <- getMap

  if (args !! 0) == "one"
    then print $ partOne parts
    else print $ partTwo parts

