module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap f (FunMonad fun) = FunMonad(f.fun)

instance Applicative FunMonad where
    pure = return
    (<*>) = mag
        where mag mf ma =   do 
                            f <- mf
                            a <- ma
                            return (f a)

instance Monad FunMonad where
    return x = FunMonad(\s -> x)
    (FunMonad fx) >>= f = FunMonad (\s -> (fun $ f $ (fx s)) $ s)