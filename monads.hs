-- This is about creating the state monad and using it in a simple program using do notation

data MyState s a = MyState (s -> (a, s))

runState :: (MyState s a) -> (s -> (a,s))
runState (MyState f) = f

put :: a -> MyState a ()
put a = MyState (\s -> ((), a))

get :: MyState s s
get = MyState (\s -> (s, s))

instance Functor (MyState s) where
    fmap f (MyState action) = MyState (\s -> let (x, s') = action s in (f x, s'))

instance Applicative (MyState s) where
    pure a = MyState (a,)
    u <*> v = MyState (\s -> let (f, s') = runState u s
                                 (x, s2) = runState v s'
                                in (f x, s2))

instance Monad (MyState s) where
    (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
    f >>= g =
        MyState (\s -> let (x, s') = runState f s in runState (g x) s')
    return :: a -> MyState s a
    return = pure

-- Takes a number from state
-- Puts that number + 9 in state
-- Takes that number
-- Returns that new number from state and add 4 to it.
mathy :: MyState Int Int
mathy = do
    x <- get
    put (x + 9)
    z2 <- get
    return (z2 + 4)

-- Takes a number puts that in the state
-- Then sends it to mathy who does something (see above)
-- Then put that in the state and return get
additive :: MyState Int Int
additive = do
    y <- get
    put y
    z <- mathy 
    put z
    get

-- This is without the grouping used by additive and mathy!
-- Obviously since this imperativly corresponds to doing the same thing in the same order it should be the same
-- Thus, we need the desired associative property
additive2 :: MyState Int Int
additive2 = do
    y <- get
    put y
    x <- get
    put (x + 9)
    z2 <- get
    z <- return (z2 + 4)
    put z
    get

-- Also, notice the use of return here, if it gave side effects other than just returning then this would not equal additive2
-- Which means that we would be unexpected and also if the right identity did not hold then the state could possibly change which means additive2
-- from z2 <- get to the return
-- This would also be unexpected behavior.
additive3 :: MyState Int Int
additive3 = do
    y <- get
    put y
    x <- get
    put (x + 9)
    z2 <- get
    let z = z2 + 4
    put z
    get

-- Finally we will show what this looks like using >>=
-- Notice the use of '_' to denote the cases where we get nothing from the state, for example, using put.
additive4 :: MyState Int Int
additive4 = get >>= (\y -> (put y) >>= (\_ -> mathy >>= (\z -> (put z >>= (\_ -> get))))) 