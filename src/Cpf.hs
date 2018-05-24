module Cpf
    ( valid
    , encode
    , decode
    ) where

type CPF = [Int]

multByPred :: [Int] -> Int -> [Int]
multByPred (x:xs) c = x * c:multByPred xs (c-1)
multByPred [] _ = [0]

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

valid :: CPF -> Bool
valid cpfNum
    | length cpfNum /= 11 = False
    | (firstDigit cpfNum) && (secondDigit cpfNum) && (not $ and $ cmpSucc cpfNum) = True
    | otherwise = False

encode :: CPF -> String
encode cpfNum = undefined

decode :: String -> CPF
decode cpfStr = undefined
