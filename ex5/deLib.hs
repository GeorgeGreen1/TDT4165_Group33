module Lib
    ( Token(..)
    , Op(..)
    , takeWhile
    , dropWhile
    , break
    , splitOn
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, dropWhile, takeWhile, break)
import Data.Char (isDigit)


-- data Maybe = Just a | Nothing deriving (Show, Eq)

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile prd [] = []
takeWhile prd xs = if (prd (head xs)) then ([(head xs)] ++ takeWhile prd (tail xs)) else []

dropWhile prd [] = []
dropWhile prd xs = if (prd (head xs)) then dropWhile prd (tail xs) else (xs)


takeWhilent prd [] = []
takeWhilent prd xs = if (not (prd (head xs))) then ([(head xs)] ++ takeWhilent prd (tail xs)) else []

dropWhilent prd [] = []
dropWhilent prd xs = if (not (prd (head xs))) then dropWhilent prd (tail xs) else (xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break prd xs = ((takeWhilent prd xs),(dropWhilent prd xs))

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el [] = []
splitOn el lst@(x:xs)
        | (x==el) =  if ((fst (break (==el) (dropWhile (==el) xs)))==[]) 
                     then [] 
                     else ((fst (break (==el) (dropWhile (==el) xs))) : 
                                (if ((snd (break (==el) (dropWhile (==el) xs))) == []) 
                                 then [] else (splitOn el (snd (break (==el) (dropWhile (==el) xs))))))
        | otherwise = (fst (break (==el) lst)) : (
                                if ((snd (break (==el) lst)) == []) 
                                then [] 
                                else (splitOn el (snd (break (==el) lst))))


data Token = TokOp Op
           | TokInt Int
           deriving (Eq, Show)

data Op = Plus
        | Minus
        | Div
        | Mult
        | Inv
        | Dup
        deriving (Show, Eq)

lex :: String -> [String]
lex str = splitOn ' ' str

tokenize :: [String] -> [Maybe Token]
tokenize ls = map (tokCh) ls 

tokCh :: String -> Maybe Token
tokCh "+" = Just (TokOp Plus)
tokCh "-" = Just (TokOp Minus)
tokCh "*" = Just (TokOp Mult)
tokCh "/" = Just (TokOp Div)
tokCh "#" = Just (TokOp Dup)
tokCh "--" = Just (TokOp Inv)
tokCh x 
    | intRun x = Just (TokInt (read x :: Int))
    | otherwise = Nothing

intRun :: String -> Bool
intRun "" = True
intRun (x:xs) 
    | x == '0' = intRun xs
    | x == '1' = intRun xs
    | x == '2' = intRun xs
    | x == '3' = intRun xs
    | x == '4' = intRun xs
    | x == '5' = intRun xs
    | x == '6' = intRun xs
    | x == '7' = intRun xs
    | x == '8' = intRun xs
    | x == '9' = intRun xs
    | otherwise = False


interpret :: [Maybe Token] -> [Maybe Token]
interpret ls = if (elem Nothing ls) then [Nothing] else (foldleft app [] ls)

foldleft :: (a -> [a] -> [a]) -> [a] -> [a] -> [a]
foldleft opp acc [] = acc
foldleft opp acc xs = opp (last xs) (foldleft opp acc (take ((length xs)-1) xs))

app (Just (TokInt a)) ls = (Just (TokInt a)) : ls 
app (Just (TokOp a)) ls@(Just (TokInt b):Just (TokInt c):_) 
    | (a == Plus) =  (Just (TokInt ((+) c b))) : (drop 2 ls)
    | (a == Minus) =  (Just (TokInt ((-) c b))) : (drop 2 ls)
    | (a == Div) =  (Just (TokInt (div c b))) : (drop 2 ls)
    | (a == Mult) =  (Just (TokInt ((*) c b))) : (drop 2 ls)
    | (a == Inv) =  (Just (TokInt (negate b))) : (tail ls)
    | (a == Dup) =  (Just (TokInt (b*2))) : (tail ls)
app (Just (TokOp a)) ls@(Just (TokInt b):[]) 
    | (a == Plus) =  Just (TokInt ((+) b 0)) : (drop 2 ls)
    | (a == Minus) =  Just (TokInt ((-) b 0)) : (drop 2 ls)
    | (a == Div) =  Just (TokInt (div b 0)) : (drop 2 ls)
    | (a == Mult) =  Just (TokInt ((*) b 0)) : (drop 2 ls)
    | (a == Inv) =  Just (TokInt (negate b)) : (tail ls)
    | (a == Dup) =  Just (TokInt (b*2)) : (tail ls)




opLeq :: Maybe Token -> Maybe Token -> Bool
opLeq (Just (TokOp a)) (Just (TokOp b))
        -- Check if ops are at the same precedence. In this case, it does not matter which has a higher precedence, so we arbitrarily choose True
        | (((a == Plus)||(a==Minus))&&((b == Plus)||(b==Minus))) || (((a == Mult)||(a==Div)||(a==Inv)||(a==Dup))&&((b == Mult)||(b==Div)||(b==Inv)||(b==Dup))) = False
        | (((a == Plus)||(a==Minus))&&((b == Mult)||(b==Div)||(b==Inv)||(b==Dup))) = False 
        | (((a == Mult)||(a==Div)||(a==Inv)||(a==Dup))&&((b == Plus)||(b==Minus))) = True
opLeq _ _ = False

shunt :: [Maybe Token] -> [Maybe Token]
shunt [Nothing] = [Nothing]
shunt ls = shuntInternal ls [] []



shuntInternal :: [Maybe Token] -> [Maybe Token] -> [Maybe Token] -> [Maybe Token]



-- Int at head
shuntInternal inp@(Just (TokInt a):_) out op = shuntInternal (tail inp) (out++[(Just (TokInt a))]) op
-- Op at head, op stack not populated
shuntInternal inp@(Just (TokOp a):_)  out [] = shuntInternal (tail inp) out [(Just (TokOp a))]
-- Op at head, op stack populated
shuntInternal inp@(Just (TokOp a):_)  out op@(Just (TokOp b):_) = if (not (opLeq (Just (TokOp a)) (Just (TokOp b)))) 
                                                      then shuntInternal inp (out ++ [Just (TokOp b)]) (tail op) 
                                                      else shuntInternal (tail inp) (out ++ [(Just (TokOp a))]) op
-- Input empty, op stack not empty
shuntInternal [] out op@(Just (TokOp a):_) = shuntInternal [] (out++[Just (TokOp a)]) (tail op)
-- Input empty, op stack empty
shuntInternal [] out [] = out
-- All empty
shuntInternal [] [] op = [Nothing]