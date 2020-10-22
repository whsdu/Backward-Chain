module AtomSpace 
    (
        Atom (..)
    )where 

import qualified MetaDefinition as M 

data Atom = Atom {getName :: M.Name}

instance M.Literal Atom where 
    literal = getName 