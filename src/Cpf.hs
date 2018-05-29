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

-- firstDigit assumes that cpfNum is well-formed
-- (!!) is making me itch!firstDigit :: [Int] -> Bool
-- XXX: If rem == 10, compare against 0
firstDigit cpfNum = rem ((sum $ multByPred (take 9 cpfNum) 10) * 10) 11 == cpfNum !! 9

-- secondDigit assumes that cpfNum is well-formed
-- XXX: If rem == 10, compare against 0
secondDigit :: [Int] -> Bool
secondDigit cpfNum = rem ((sum $ multByPred (take 10 cpfNum) 11) * 10) 11 == cpfNum !! 10 

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
