module AtomSpace 
    (
        Atom (..)
    )where 

import qualified MetaDefinition as M 

newtype Atom = Atom {atomName :: M.Name} deriving Show 

instance M.Literal Atom where 
    literal = atomName

