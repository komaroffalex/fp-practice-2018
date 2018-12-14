module Task3_3 where

newtype UnionPSet a = UnionPSet{ uContains :: (a -> Bool) }
newtype IntersectPSet a = IntersectPSet{ iContains :: (a -> Bool) }
newtype DiffPSet a = DiffPSet{ oContains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Правило, которому должны следовать Моноиды:
-- x `mappend` mzero === mzero `mappend` x === x
-- Для успешной реализации моноида необходимо чтобы этот закон выполнялся.

--Объединение множеств
instance Semigroup (UnionPSet a) where
        (UnionPSet left) <> (UnionPSet right) = UnionPSet (\x -> (left x) || (right x))

instance Monoid (UnionPSet a) where
        mempty = UnionPSet (\_ -> False)
        mappend = (<>)

--Пересечение множеств
instance Semigroup (IntersectPSet a) where
        (IntersectPSet left) <> (IntersectPSet right) = IntersectPSet (\x -> (left x) && (right x))

instance Monoid (IntersectPSet a) where
        mempty = IntersectPSet (\_ -> False)
        mappend = (<>)

--Симметричная разность множеств.
--Симметричной разностью множеств А и В называется множество А Δ В, 
--являющееся объединением разностей множеств АВ и ВА.
instance Semigroup (DiffPSet a) where
        (DiffPSet left) <> (DiffPSet right) = DiffPSet (\x -> (not $ left x) && (not $ right x))

instance Monoid (DiffPSet a) where
        mempty = DiffPSet (\_ -> True)
        mappend = (<>)

--Так как требуемая сигнатура функтора имеет вид:
-- fmap :: Functor f => (a -> b) -> f a -> f b
--Реализация такой сигнатуры требует наличия информации
--о типе переменной. Так как такой информации нет - 
--реализовать функтор не получится.
--Функция fmap предполагает, что один из типов фиксирован:
--т.е для функции получится вот так
--instance Functor ((->) a) where
--    fmap = ..
--тогда если мы напишем fmap для функции
--instance Functor ((->) a) where
--fmap f x  = f . x
--по-простому полчится просто композиция функций
--у нас же в задании получаетс обертка над (a -> Bool)
--Если для такой прибдлуды попытаться написать функтор, то выйдет что-то вроде
-- fmap :: Functor f => ((a -> Bool) -> (b -> Bool)) -> f (a -> Bool) -> f (b -> Bool)
--Нам как-то у функции будет подменить ее входной параметр
--который мы подменять не можем
--мы можем подменять только выходной