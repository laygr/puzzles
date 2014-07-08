-- ABCD + BCAD = FAEC
-- All letters represent different digits

module Main where

import Data.List
import Control.Monad

main = mapM print result

listToInt x = sum $ map (\pair -> snd pair * 10 ^ fst pair) (zip [3,2,1,0] x)

intToList x = reverse $ intToListAux x
intToListAux 0 = []
intToListAux x = x `mod` 10 : intToListAux (x `div` 10)

elementsFromList code list = map (\n -> list !! n) code

result = do
    permutation <- permutations [0..9]
    let
        abcdList = elementsFromList [0,1,2,3] permutation
        bcadList = elementsFromList [1,2,0,3] permutation
        abcd = listToInt abcdList
        bcad = listToInt bcadList
        addition = abcd + bcad
        additionList = intToList addition
        f = additionList !! 0
        a = additionList !! 1
        e = additionList !! 2
        c = additionList !! 3
            in
                guard ( notElem f abcdList  && a == (abcdList !! 0)  &&
                        notElem e abcdList  && c == (abcdList !! 2)  && f /= e)
                      >> return (show abcd ++ "+" ++ show bcad ++ "=" ++ show addition)