module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

instance Foldable ReverseList where
    foldr f a RNil = a
    foldr f a (RCons xs x) = f x (foldr f a xs)

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons l r) = r : rlistToList l

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList list = foldl (\ l r -> RCons l r) RNil list

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil l) = show l
    show (RCons l r) = show l ++ "," ++ show r

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) (RCons l r) (RCons l' r') = l == l' && r == r'
    (==) _ _ = False

instance (Ord a) => Ord (ReverseList a) where
    (<=) l r = rlistToList l <= rlistToList r 

instance Semigroup (ReverseList a) where
    (<>) xs ys = foldr (\x s -> RCons s x) ys xs

instance Monoid (ReverseList a) where
    mempty = RNil    

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons l r) = RCons (fmap f l) (f r) 