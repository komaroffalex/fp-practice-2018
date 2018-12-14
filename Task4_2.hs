module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf a1 b1 c1 d1) (FourOf a2 b2 c2 d2) =
        FourOf (a1 a2) (b1 b2) (c1 c2) (d1 d2)    

instance Monad FourOf where
    return x = FourOf x x x x
    (FourOf a b c d) >>= f = FourOf (firstEl(f a)) (secEl(f b)) (thirdEl(f c)) (fourthEl(f d))
        where   firstEl (FourOf a _ _ _) = a
                secEl (FourOf _ b _ _) = b
                thirdEl (FourOf _ _ c _) = c
                fourthEl (FourOf _ _ _ d) = d

