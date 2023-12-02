import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.Maybe

getMap :: IO [String]
getMap = do
  inp <- getContents
  return $ lines inp

partOne :: [String] -> Int
partOne = fromMaybe 0 . fmap sum . traverse (ints . firstAndLast)
  where
    ints :: [Maybe Char] -> Maybe Int
    ints = liftM read . sequence

partTwo parts = ""

firstAndLast :: String -> [Maybe Char]
firstAndLast str = [firstDigit str ['1'..'9'], lastDigit str ['1'..'9']]

firstDigit :: String -> [Char] -> Maybe Char
firstDigit str []     = Nothing
firstDigit str (i:is) = Just $ first i (firstDigit str is)
  where
    first a Nothing  = a
    first a (Just b) = lowest (a, (elemIndex a str)) (b, (elemIndex b str))

    lowest (_, Nothing) (b, j) = b
    lowest (a, i) (b, Nothing) = a
    lowest (a, i) (b, j)       | i < j     = a
                               | otherwise = b

lastDigit :: String -> [Char] -> Maybe Char
lastDigit str []     = Nothing
lastDigit str (i:is) = Just $ last i (lastDigit str is)
  where
    last a Nothing  = a
    last a (Just b) = biggest (a, (elemIndex a str)) (b, (elemIndex b str))

    biggest (_, Nothing) (b, j) = b
    biggest (a, i) (b, Nothing) = a
    biggest (a, i) (b, j)       | i < j     = b
                                | otherwise = a

main = do
  args  <- getArgs
  parts <- getMap

  if (args !! 0) == "one"
    then print $ partOne parts
    else print $ partTwo parts

