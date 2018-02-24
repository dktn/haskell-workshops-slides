module Examples where

main :: IO ()
main = putStrLn "Hello World!"

-- using let ... in
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        larger  = quicksort [a | a <- xs, a >  x]
    in  smaller ++ [x] ++ larger

-- using where
quicksort' :: Ord a => [a] -> [a]
quicksort' []     = []
quicksort' (x:xs) = smaller ++ [x] ++ larger
  where
    smaller = quicksort' [a | a <- xs, a <= x]
    larger  = quicksort' [a | a <- xs, a >  x]

max' :: Ord a => a -> a -> a
max' a b =
  if a >= b
    then a
    else b

max'' :: Ord a => a -> a -> a
max'' a b
    | a >= b    = a
    | otherwise = b

maximum' :: Ord a => [a] -> a
maximum' []   = error "why?"
maximum' [x]  = x
maximum' (x:xs)
    | x > tailMaximum = x
    | otherwise = tailMaximum
    where tailMaximum = maximum' xs

maximum'' :: Ord a => [a] -> a
maximum'' []   = error "why?"
maximum'' [x]  = x
maximum'' (x:xs) =
    if x > tailMaximum
        then x
        else tailMaximum
    where tailMaximum = maximum'' xs

maximumTotal :: Ord a => [a] -> Maybe a
maximumTotal []   = Nothing
maximumTotal [x]  = Just x
maximumTotal (x:xs)
    | Just x > tailMaximum = Just x
    | otherwise       = tailMaximum
    where tailMaximum = maximumTotal xs

minimumTotal :: Ord a => [a] -> Maybe a
minimumTotal []   = Nothing
minimumTotal [x]  = Just x
minimumTotal (x:xs)
    | Just x < tailMinimum = Just x
    | otherwise       = tailMinimum
    where tailMinimum = minimumTotal xs


data Tree = Leaf
    | Node Tree Tree
    deriving (Show, Eq)

longestBranch :: Tree -> Int
longestBranch Leaf         = 0
longestBranch (Node b1 b2) = 1 + max (longestBranch b1) (longestBranch b2)

shortestBranch :: Tree -> Int
shortestBranch Leaf         = 0
shortestBranch (Node b1 b2) = 1 + min (shortestBranch b1) (shortestBranch b2)

isSubtree :: Tree -> Tree -> Bool
isSubtree Leaf Leaf          = False
isSubtree (Node _ _) Leaf    = False
isSubtree t1 t2@(Node b1 b2) = t1 == t2 || isSubtree t1 b1 || isSubtree t1 b2


