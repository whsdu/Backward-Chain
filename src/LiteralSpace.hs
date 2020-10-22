{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module LiteralSpace 
    (
        Literal(..)
    ) where 

import qualified MetaDefinition  as M 

data Literal = forall a . M.Literal a => Literal a

