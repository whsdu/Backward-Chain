{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MetaDefinition where 

type Name = String 

data Imp = S | D 

class Literal a where 
    literal :: a -> Name 

class (Literal a, Literal b) => Negation a b where 
    negation :: a -> b -> Bool 
    neg :: a -> b 

