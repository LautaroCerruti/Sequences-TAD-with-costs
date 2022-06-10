module ListSeq where

import Par
import Seq

contraerSL f [] = []
contraerSL f l@[x] = l
contraerSL f (x:y:xs) = let (z:zs) =  f x y ||| contraerSL f xs
                         in z:zs

instance Seq [] where
    emptyS = []

    singleton a = [a]

    lengthS = length

    nthS = (!!)

    tabulateS f n = tabulateSaux n
        where 
            tabulateSaux 0 = emptyS
            tabulateSaux k = let (x, xs) = f (n-k) ||| tabulateSaux (k-1)
                in x:xs 

    mapS f [] = emptyS
    mapS f (x:xs) = let (y,ys) = f x ||| mapS f xs
        in y:ys

    filterS f [] = emptyS
    filterS f (x:xs) = let (y,ys) = f x ||| filterS f xs
        in if y then x:ys else ys

    appendS [] ys = ys
    append xs [] = xs
    append (x:xs) ys = x:(append xs ys)

    takeS l n = take n l

    dropS l n = drop n l

    showtS [] = EMPTY
    showtS [a] = ELT a
    showtS xs = let half = div (lengthS xs) 2
                    (l,r) = takeS xs half ||| dropS xs half
                in NODE l r

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS = concat

    reduceS f a [] = a
    reduceS f a [x] = f x a 
    reduceS f a xs = reduceS f a (contraerSL f xs)
