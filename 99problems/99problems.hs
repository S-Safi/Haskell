{-# OPTIONS_GHC -Wall #-}



{-
Problem 1
(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a
myLast [] = error "Empty List"
myLast xs = xs !! ((length xs) - 1)

myLast' :: [a] -> a
myLast' [] = error "Empty List"
myLast' [x] = x
myLast' (_:xs) = myLast' xs 

myLast'' :: [a] -> a
myLast'' = head . reverse

myLast''' :: [a] -> a
myLast''' xs = head (reverse xs)

myLast'''' :: [a] -> a
myLast'''' = foldr1 (const id)



{-
Notes:
"." is read as "compose" <--- need to remember this term

Compose is just a way to join two functions together

"." also means "after"

e.g 
f . g means f after g

so 
head . reverse  =  head after reverse

which is the same as 
myfunction x = f (g x)
-}


{-
foldr
means "fold right"
example:
> foldr (+) 0 [1, 2, 3]
-}

{- PROBLEM 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'

-}

myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [_] = error "Needs 2 values in list"
myButLast xs = xs !! ((length xs) - 2)

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

myButLast'' :: [a] -> a
myButLast'' xs = head (tail (reverse xs))

myButLast''' :: [a] -> a
myButLast''' x = reverse x !! 1

myButLast'''' :: [a] -> a
myButLast'''' [] = error "Empty List"
myButLast'''' [_] = error "requires 2 or more values"
myButLast'''' [x,_]  = x
myButLast'''' (_:xs) = myButLast'' xs

