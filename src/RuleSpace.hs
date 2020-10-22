module RuleSpace 
    (
        Rule (..)
    )
    where 


import qualified MetaDefinition  as M 
import LiteralSpace (Literal(..))


data Rule = 
    Rule 
    { getName :: M.Name 
    , getImp :: M.Imp 
    , getBogy :: [Literal]
    , getHead :: Literal 
    -- , getHead :: L.LiteralSpace RuleSpace
    }

instance M.Literal Rule where 
    literal r = getName r 