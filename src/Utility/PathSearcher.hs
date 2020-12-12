{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}
module Utility.PathSearcher where 

import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)

import qualified Space.Language as L 
import qualified Space.Meta as M 
import Env 
import qualified Utility.Ordering  as O
import qualified Utility.Language as LU 


{-
The Rule Level Graph is being represented with a list of rules.
Following function server the purpose of:
1. Find a list of rules that eventually compose an argument. 
2. Find different lists of rules that equivalent to different arguments of the same conclusion.
-}
concludeBy :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.Language 
concludeBy l = do 
    dRules <- grab @L.DefeasibleRules
    sRules <- grab @L.StrictRules
    let rules = L.getDefeasibleRules dRules ++ L.getStrictRules sRules
    pure [r | r <- rules , L.conC r == l]

queryEquifinalPathSection :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    , Traversable t 
    ) =>t L.Literal -> m L.EquifinalPathSections
queryEquifinalPathSection bodies = do 
    subLevel <- mapM concludeBy bodies 
    pure $ foldr createParallel [[]] subLevel 
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                pa <- paths
                a <- ls 
                pure $  pa:a 

queryEquifinalPaths :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Language -> m L.EquifinalPaths 
queryEquifinalPaths rules = do 
    let bodies = concat $ L.body <$> rules 
    if null bodies
        then pure [[rules]]
        else 
            do 
                equifinality <- queryEquifinalPathSection bodies 
                rs <- mapM queryEquifinalPaths equifinality 
                pure $ 
                    do
                        r <- concat rs 
                        pure $ rules : r

querySingleConclusion :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.EquifinalPaths 
querySingleConclusion conC = do 
    cs <- concludeBy conC 
    let 
        dos = (:[]) <$> cs 
    rs <- mapM queryEquifinalPaths dos 
    pure $ concat rs 


{-Given: 
1. The rule level graph representation ( the List of Rule)
2. The prefernceMap that can be used to compute the preference relation between two arugment. 
find the 
-}
evenLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
evenLayer lang pMap (p:ps) = 
    let
        nextLayerAttackers = queryNextLayerRebut lang pMap p
        defeaders = oddLayer lang pMap <$> nextLayerAttackers
        succDefeaders = filter null defeaders 
    in if length succDefeaders == length nextLayerAttackers then p : concat defeaders else evenLayer lang pMap ps 
evenLayer lang pMap [] = []

-- | The purpose of this function is to guarantee that all input equifinal paths are defeaded.
--  `ps` is the equifinalPaths (Arguments)  that attack some upper layer argument(sub-argument)
-- 
oddLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
oddLayer lang pMap ps = 
    let 
        oddAttackers = queryNextLayerRebut lang pMap <$> ps 
        succAttackers = filter null oddAttackers
    in 
        if 
            length succAttackers /= length ps 
            then [] 
            else  
                concat $ evenLayer lang pMap <$> concat oddAttackers

queryNextLayerRebut :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
queryNextLayerRebut lang pMap p  = 
    let 
        conjunctiveRules = concat p 
        conjunctiveConcs = L.conC <$> conjunctiveRules 
    in queryNextLayerAttack lang pMap conjunctiveRules <$> conjunctiveConcs

-- | TODOs: 
-- What if to separate Equifinal Paths that disjunctively support `neg l`
-- However one of them has a circle ? 
-- 1. Change the name to undercut
queryNextLayerUndercut :: L.Language -> L.Literal -> L.EquifinalPaths
queryNextLayerUndercut lang l = equifinalPathForQuery lang $ M.neg l 

-- | `conC` is an conclusion from upper layer 
-- 1. get the path to `conC` (PathC), there is only one path, because it is a section of one equifinal path. 
-- 2. get equifinal paths to `neg conC`
-- 3. filter the paths of `neg conC` , select these can defeat (PathC).
-- returns: Arguments (Equifinal Paths) that successfully defeat `conC`.
-- TODOs: 
-- 1. Change the name : Attack in this case is defead as rebut here 
queryNextLayerAttack :: L.Language -> L.PreferenceMap -> L.Language -> L.Literal -> L.EquifinalPaths 
queryNextLayerAttack lang pMap pathRuls conC = 
    let
        argPath = getArgPath pathRuls conC 
        qConc = M.neg conC 
        attackPaths = equifinalPathForQuery lang qConc 
        attackerPaths = filter  (`defeat` argPath) attackPaths
    in attackerPaths 


{-
---- | below is the original implementation of Equifinal Paths
-}
equifinalPathForQuery' :: L.Language -> L.Literal -> L.EquifinalPaths
equifinalPathForQuery' lang conC= 
    let 
        dos = (:[]) <$> concludedBy' lang conC
    in concat $ equifinalPaths' lang <$> dos

concludedBy' :: L.Language -> L.Literal -> L.Language
concludedBy' lang l = [r | r <- lang , L.conC r == l]

equifinalPathSections' :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> L.EquifinalPathSections
equifinalPathSections' lang bodies =  
    let subLevel = concludedBy' lang <$> bodies 
    in foldr createParallel [[]] subLevel 
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                path <- paths
                a <- ls 
                pure $  path:a 

equifinalPaths' :: L.Language -> L.Language -> L.EquifinalPaths
equifinalPaths' lang sRules = 
    let bodies = concat $ L.body <$> sRules
    in 
        if null bodies
            then [[sRules]]
        else 
            let equifinality = equifinalPathSections' lang bodies  
            in
                do 
                    b <-  concat $ equifinalPaths' lang <$> equifinality 
                    pure $ sRules : b