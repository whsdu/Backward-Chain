module MetaDefinition where 


type Name = String 

data Imp = S | D | N

instance Show Imp where 
    show S = "->"
    show D = "=>"
    show N = " "

instance Eq Imp where 
    (==) S S = True 
    (==) D D = True
    (==) _ _ = False

class (Eq a) => Negation a where 
    neg :: a -> a 
    negation :: a -> a -> Bool 
    negation a1 a2 = neg a1 == a2
