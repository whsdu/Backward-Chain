module Algorithm.BC where

import Control.Monad.Reader 

import qualified Space.Language as L 
import qualified Space.Argumentation as A
import qualified Space.Meta as M 

import qualified Utility.Argumentation as AU
import qualified Utility.Language as LU
import Env 


-- rulesForLiteral :: 
--     ( MonadReader env m 
--     , Has env L.Language
--     , MonadIO m 
--     ) => L.Language -> [M.Name] -> Literal -> ArgumentationSpace
-- rulesForLiteral language argNames l =
--     let
--         prod = subBodys language l
--     in parsBasicArgument prod argNames
-- 

-- funcASG :: [Literal] -> Literal -> [Literal]
-- funcASG language l =
--     let
--         al = subBodys language l
--     in computeASG language al al
--     where
--         computeASG lag initAL endAL =
--             let
--                 attackConc = rmdups . concat $ subBodys language . neg . L.conC <$> initAL
--                 negRuleAsHead = concat $ ruleAsConc language . neg <$> attackConc ++ initAL
--                 attack = rmdups . concat $ subBodys language <$> negRuleAsHead ++ attackConc ++ initAL
--             in
--                 if attack == endAL
--                     then attack
--                     else computeASG lag attack attack
-- 