module Cpf
    ( valid
    , encode
    , decode
    ) where

import Data.Char (isNumber)

type CPF = [Int]

multByPred :: [Int] -> Int -> [Int]
multByPred (x:xs) c = x * c:multByPred xs (c-1)
multByPred [] _ = []

-- compareDigit takes a cpfNum (CPF), i (index to take from original cpfNum) 
-- , ci (index of number to compare against), x (counter for multByPred)
-- compareDigit assumes that cpfNum is well-formed
compareDigit :: [Int] -> Int -> Int -> Int -> Bool
compareDigit cpfNum i ci x = case remainder of
                             10 -> 0 == cpfNum !! ci
                             _ -> remainder == cpfNum !! ci
                             where remainder = rem ((sum $ multByPred (take i cpfNum) x) * 10) 11

-- compares remainder of the firsts nine digits against first digit after "-"
firstDigit :: [Int] -> Bool
firstDigit cpfNum = compareDigit cpfNum 9 9 10

-- compares remainder of the firsts ten digits against second digit after "-"
secondDigit :: [Int] -> Bool
secondDigit cpfNum = compareDigit cpfNum 10 10 11

cmpSucc (x:xs) = if xs /= []
                    then (x == xs !! 0):cmpSucc xs 
                    else []

normalize :: String -> String
normalize = filter isNumber

convert :: String -> CPF
convert (x:xs) = (read [x] :: Int):convert xs
convert [] = []

slice :: Int -> Int -> [a] -> [a]
slice s t xs = take t $ drop s xs

toString :: Show a => [a] -> String
toString (x:xs) = show x ++ toString xs
toString [] = []

valid :: CPF -> Bool
valid cpfNum
    | length cpfNum /= 11 = False
    | (firstDigit cpfNum) && (secondDigit cpfNum) && (not $ and $ cmpSucc cpfNum) = True
    | otherwise = False

encode :: CPF -> String
encode cpfNum = fstTriplet ++ "." ++ sndTriplet ++ "." ++ thrTriplet ++ "-" ++ endDigits where
    fstTriplet = toString $ slice 0 3 cpfNum
    sndTriplet = toString $ slice 3 3 cpfNum
    thrTriplet = toString $ slice 6 3 cpfNum
    endDigits = toString $ slice 9 2 cpfNum

decode :: String -> CPF
decode = (convert . normalize)
