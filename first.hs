-- 1, 6, 21, 66, ...
sequenceIntegers :: Num t => t -> [t]
sequenceIntegers n = n : sequenceIntegers (3*(n + 1))

filterMs :: (a -> Bool) -> [a] -> [a]
filterMs f [] = []
filterMs f (x:xs) = if f x then x:filterMs f xs else filterMs f xs

headMs :: [a] -> a
headMs (x:xs) = x

evenMs :: Integral a => a -> Bool
evenMs t = mod t 2 == 0