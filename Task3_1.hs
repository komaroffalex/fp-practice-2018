module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero = "(Zero())"
    show (Succ wpn) = "(Succ" ++ show wpn ++ ")"
    show (Pred wpn) = "(Pred" ++ show wpn ++ ")"

readWpn :: [Char] -> WeirdPeanoNumber
readWpn ('(' : wpn0 : wpn1 : wpn2 : wpn3 : others) 
    | [wpn0, wpn1, wpn2, wpn3] == "Zero" = Zero
    | [wpn0, wpn1, wpn2, wpn3] == "Succ" = Succ (readWpn others)
    | [wpn0, wpn1, wpn2, wpn3] == "Pred" = Pred (readWpn others)
    | otherwise = error "Not a WeirdPeanoNumber"

instance Read WeirdPeanoNumber where
    readsPrec _ str = [(readWpn str, "")]

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) l r = False

instance Ord WeirdPeanoNumber where
    (<=) Zero Zero = True
    (<=) Zero (Succ r)  | (r < (-1)) = False 
                        | otherwise = True
    (<=) Zero (Pred r)  | (r < 1) = False 
                        | otherwise = True
    
    (<=) (Pred l) (Pred r) = (l <= r)
    (<=) (Succ l) (Succ r) = (l <= r)
    
    (<=) (Pred l) r = toInteger (l - 1) <= toInteger r
    (<=) (Succ l) r = toInteger (l + 1) <= toInteger r
    
    (<) Zero Zero = False
    (<) Zero (Succ r)   | (r < 0) = False 
                        | otherwise = True
    (<) Zero (Pred r)   | (r > 1) = True 
                        | otherwise = False
    
    (<) (Pred l) (Pred r) =  (l < r) 
    (<) (Succ l) (Succ r) =  (l < r) 
    
    (<) (Pred l) r = toInteger (l - 1) < toInteger r
    (<) (Succ l) r = toInteger (l + 1) < toInteger r

    (>) l r = not ((<) l r)
    (>=) l r = not ((<=) l r)

instance Num WeirdPeanoNumber where    
    negate Zero = Zero
    negate (Pred l) = Succ (negate l)
    negate (Succ l) = Pred (negate l)    

    signum Zero = Zero
    signum (Succ l)         | (Succ l) > Zero = Succ Zero
                            | (Succ l) < Zero = Pred Zero
                            | otherwise = Zero
    signum (Pred l)         | (Pred l) > Zero =  Succ Zero
                            | (Pred l) < Zero = Pred Zero
                            | otherwise = Zero

    fromInteger l   | (l == 0) = Zero
                    | (l > 0) = Succ $ fromInteger (l - 1)
                    | (l < 0) = Pred $ fromInteger (l + 1)

    abs l       | (signum l < Zero) = negate l 
                | otherwise = l

    (+) l Zero = l
    (+) Zero r = r
    (+) (Pred l) (Pred r) = l + Pred (Pred r)
    (+) (Succ l) (Succ r) = l + Succ (Succ r)
    (+) (Pred l) (Succ r) = l + r
    (+) (Succ l) (Pred r) = l + r

    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ Zero) r = r
    (*) l (Succ Zero) = l
    (*) (Succ l) (Succ r) = (Succ l) * r + (Succ l)
    (*) (Pred l) (Pred r) = negate (Pred l) * negate (Pred r)
    (*) (Succ l) (Pred r) = negate ((Succ l) * negate (Pred r))
    (*) (Pred l) (Succ r) = negate (negate (Pred l) * (Succ r))

instance Enum WeirdPeanoNumber where
    toEnum l    | l == 0 = Zero
                | l > 0 = Succ( toEnum (l - 1))
                | l < 0 = Pred( toEnum (l + 1))

    fromEnum Zero = 0
    fromEnum (Succ l) = fromEnum l + 1
    fromEnum (Pred l) = fromEnum l - 1

instance Real WeirdPeanoNumber where
    toRational Zero = toRational 0
    toRational (Succ l) = toRational (toRational l + 1)
    toRational (Pred l) = toRational (toRational l - 1)

instance Integral WeirdPeanoNumber where
    toInteger Zero = toInteger 0
    toInteger (Succ l) = toInteger (toInteger l + 1)
    toInteger (Pred l) = toInteger (toInteger l - 1)

    quotRem l Zero = error "Can not be devided by zero"
    quotRem l r | (l == r) = (Succ (Zero), Zero)
                | abs l < abs r = (Zero, l)
                | l > 0 && r > 0 = quotrem' l r
                | otherwise = quotrem' (abs l) (abs r)
    
                where quotrem' l r | (l == r) = (Succ(Zero), Zero)
                                    | (l < r)  = (Zero, l)
                                    | (l > r)  = (Succ(fst (quotrem' (l-r) r)),snd (quotrem' (l-r) r))