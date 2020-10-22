{-# LANGUAGE GADTs #-}
module MetaDefinition where 

type Name = String 

data Imp = S | D 

class Literal a where 
    literal :: a -> Name 