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

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile prd [] = []
takeWhile prd xs = if (prd (head xs)) then ([(head xs)] ++ takeWhile prd (tail xs)) else []

dropWhile prd [] = []
dropWhile prd xs = if (prd (head xs)) then dropWhile prd (tail xs) else (xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break prd xs = ((takeWhile prd xs),(dropWhile prd xs))

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el [] = [[]]
splitOn el xs =  (if ((length bgn)==0) then [] else [bgn]) ++ (if ((length nxt)==0) then [] else (splitOn el nxt))
    where lst = (dropWhile ((==) el) xs)
          bgn = (fst (break ((/=) el) lst))
          nxt = (snd (break ((/=) el) lst))

data Token = TokOp Op
           | TokInt Int
           | TokErr
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

tokenize :: [String] -> [Token]
tokenize ls = map (tokEl) ls 
tokEl :: String -> Token
tokEl x = tokCh x
tokCh :: String -> Token
tokCh "+" = TokOp Plus
tokCh "-" = TokOp Minus
tokCh "*" = TokOp Mult
tokCh "/" = TokOp Div
tokCh "#" = TokOp Dup
tokCh "--" = TokOp Dup
tokCh x 
    | x == "0" = TokInt 0
    | x == "1" = TokInt 1
    | x == "2" = TokInt 2
    | x == "3" = TokInt 3
    | x == "4" = TokInt 4
    | x == "5" = TokInt 5
    | x == "6" = TokInt 6
    | x == "7" = TokInt 7
    | x == "8" = TokInt 8
    | x == "9" = TokInt 9
    | otherwise = TokErr

interpret :: [Token] -> [Token]
interpret ls = foldleft app [] ls

foldleft :: (a -> [a] -> [a]) -> [a] -> [a] -> [a]
foldleft opp acc [] = acc
foldleft opp acc xs = opp (last xs) (foldleft opp acc (take ((length xs)-1) xs))

--app :: Token -> [Token] -> [Token]

-- Find a way to split this up correctly

app (TokInt a) ls = (TokInt a) : ls 
--app (TokOp a) ls@(TokInt b:TokInt c:_) = (TokInt ((getOp a) b c)) : (drop 2 ls)
app (TokOp a) ls@(TokInt b:TokInt c:_) 
    | (a == Plus) =  (TokInt ((+) b c)) : (drop 2 ls)
    | (a == Minus) =  (TokInt ((-) b c)) : (drop 2 ls)
    | (a == Div) =  (TokInt (div b c)) : (drop 2 ls)
    | (a == Mult) =  (TokInt ((*) b c)) : (drop 2 ls)
    | (a == Inv) =  (TokInt (negate b)) : (tail ls)
    | (a == Dup) =  (TokInt (b*2)) : (tail ls)
app (TokOp a) ls@(TokInt b:[]) 
    | (a == Plus) =  (TokInt ((+) 0 b)) : (drop 2 ls)
    | (a == Minus) =  (TokInt ((-) 0 b)) : (drop 2 ls)
    | (a == Div) =  (TokInt (div 0 b)) : (drop 2 ls)
    | (a == Mult) =  (TokInt ((*) 0 b)) : (drop 2 ls)
    | (a == Inv) =  (TokInt (negate b)) : (tail ls)
    | (a == Dup) =  (TokInt (b*2)) : (tail ls)

-- getOp (TokOp Plus) = (+)
-- getOp (TokOp Minus) = (-)
-- getOp (TokOp Mult) = (*)
-- getOp (TokOp Div) = (/)
-- parseTokens TokOp = 
-- parseTokens lst@(Token a:_)  = TokInt ((getOp a) )
-- -- (Farthest right) ((Farthest right with any num) Farthest left) (Recurse farthest in)
-- -- Farthest right is operator/function, return value in the side opposite

-- foldl op acc [TokInt a] = TokInt (acc op a)
-- foldl op acc xs = op () ()

-- -- xs ends in an int
-- foldl op acc (_:TokInt a) = op () a




opLeq :: Token -> Token -> Bool
opLeq = undefined

shunt :: [Token] -> [Token]
shunt = undefined

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal = undefined
