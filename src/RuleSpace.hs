{-# LANGUAGE ExistentialQuantification #-}
module RuleSpace 
    (
        Rule (..)
    )
    where 


import qualified MetaDefinition  as M 

data Rule = forall a . (M.Literal a) =>  
    Rule 
    { ruleName :: M.Name 
    , getImp :: M.Imp 
    , getBody :: [a]
    , getHead :: a
    -- , getHead :: L.LiteralSpace RuleSpace
    } 

instance M.Literal Rule where 
    literal  = ruleName 

instance Show Rule where 
    show Rule{..} = 