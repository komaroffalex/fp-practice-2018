module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

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
    (<>) a b = mappend a b

instance Monoid (ReverseList a) where
    mempty = RNil    

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons l r) = RCons (fmap f l) (f r) 