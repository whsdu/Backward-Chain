# DT
1. stack ghci
1. use `:m` to clean the import space
1. import necessary modules:
    ```
    import qualified Space.Language as L
    import qualified Space.Argumentation as A
    import Space.Meta as M
    import qualified Utility.Language as LU
    import qualified Utility.Argumentation as AU
    import qualified Data.HashMap.Strict as Map
    import qualified GRI.GRI as GRI
    import qualified Algorithm.BC as BC
    import Data.Maybe
    import Parser.FileParser
    import Arena
    import Env
    ```
1. head (conC), name, imp, body 
    ```
    L.conC r4
    L.name r4
    L.imp r4
    L.imp r6
    L.imp a 
    L.body r4
    L.body r6
    L.body r7
    ```
1. applicable 
    ```
    r7
    LU.isApplicable r7 [c,t]
    LU.isApplicable r7 [c,t,r3,r7]
    LU.isApplicable r7 [c,r3,r6]
    ```
1. consistent
    ```
    demoLanguage
    LU.isConsistent demoLanguage
    let nb = neg b
    LU.isConsistent [a,b,c]
    LU.isConsistent [a,b,c,nb]
    ```