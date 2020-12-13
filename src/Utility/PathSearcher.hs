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


{- Backward Chaining Search
Given: 
1. The rule level graph representation (the List of Rules).
2. The prefernceMap that can be used to compute the preference relation between two arugment. 
find the 
-}

-- | select one path from the list of equifinal paths.
-- If no one attacks this path, return this path
-- If it is attacked by some other equifinalpaths:
--    check if it is defeated
--    if this path is not defeated by any one, return this path together with all next-level equifinal paths that attack it.
-- otherwise check the next path in the list of equifinal paths. 
-- If no path in the list of equifinal paths pass the check, return empty []. 
-- `attackGroup` is of type [EquifinalPaths], each EquifinalPaths attack certain part of the same path.
evenLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
evenLayer' lang pMap (p:ps) = 
    let
        attackGroup = queryPathAttackers lang pMap p 
        defenders = oddLayer' lang pMap <$> attackGroup
        succDefeaders = filter (not . null) defenders
    in if length succDefeaders == length attackGroup then [p] : concat succDefeaders else evenLayer' lang pMap ps 
evenLayer' _ _ [] = []


-- | every path in this list of equifinal paths should be defeated 
-- If all these paths are defeated then return these path together with the next layer defeaders path 
-- If not all these path are dfeated then return [].
-- oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
oddLayer' lang pMap ps = 
    let 
        attackGroups = [ (p , queryPathAttackers lang pMap p) | p <- ps , (not . null) (queryPathAttackers lang pMap p)]
    in if length attackGroups /=  length ps 
        then [] 
        else concat $ checkAttackGroup lang pMap <$> attackGroups
    where 
        checkAttackGroup :: L.Language -> L.PreferenceMap -> (L.Path ,[L.EquifinalPaths]) -> [L.EquifinalPaths]
        checkAttackGroup lang fMap (path, group) = 
            let 
                defenderGroup = concat (evenLayer' lang fMap <$> group)
            in 
                if not . null $ defenderGroup  then [path] : defenderGroup else [] 

queryPathAttackers :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
queryPathAttackers lang pMap path = 
    let 
        nextLayerRebutting = queryPathRebuts' lang pMap path
        nextLayerUndercutting = queryPathUndercut' lang path 
    in nextLayerRebutting ++ nextLayerUndercutting

-- | TODOs: 
-- What if to separate Equifinal Paths that disjunctively support `neg l`
-- However one of them has a circle ? 
-- 1. Change the name to undercut
queryPathUndercut' :: L.Language -> L.Path -> [L.EquifinalPaths]
queryPathUndercut' lang p = 
    let 
        defeasibleRules = [r | r <- concat p, L.imp r == M.D && (not . null) (L.body r)]
        undercutted = M.neg <$> defeasibleRules
    in  equifinalPathForQuery' lang <$> undercutted 

-- | Only defeasible Literals can be rebut, includes:
-- 1. defeasible rules 
-- 2. ordinary premises
queryPathRebuts' :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
queryPathRebuts' lang pMap p  = 
    let 
        defeasible = [r | r <- concat p , L.imp r == M.D]
        conjunctiveConcs = L.conC <$> defeasible
    in queryConcRebuts' lang pMap defeasible <$> conjunctiveConcs

-- | Given conclusion c
-- Get all Equifinal paths of neg c
-- select paths that defeat path of c.
-- TODO: 
-- Ordering functions is hard coded here. 
queryConcRebuts' :: L.Language -> L.PreferenceMap -> L.Language -> L.Literal -> L.EquifinalPaths 
queryConcRebuts' lang pMap pathRuls conC = 
    let
        qConc = M.neg conC 
        argPath = head $ equifinalPathForQuery' pathRuls conC  -- This part do not need to lift to monad level
        attackPaths = equifinalPathForQuery' lang qConc 
    in [p | p <- attackPaths, O.weakestLink pMap O.dem p argPath] -- This is problematic to convert to monad level 


{-Old Implementation-}

-- evenLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
-- evenLayer lang pMap (p:ps) = 
--     let
--         nextLayerAttackers = queryNextLayerRebut lang pMap p
--         defeaders = oddLayer lang pMap <$> nextLayerAttackers
--         succDefeaders = filter null defeaders 
--     in if length succDefeaders == length nextLayerAttackers then p : concat defeaders else evenLayer lang pMap ps 
-- evenLayer lang pMap [] = []

-- -- | The purpose of this function is to guarantee that all input equifinal paths are defeaded.
-- --  `ps` is the equifinalPaths (Arguments)  that attack some upper layer argument(sub-argument)
-- -- 
-- oddLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
-- oddLayer lang pMap ps = 
--     let 
--         oddAttackers = queryNextLayerRebut lang pMap <$> ps 
--         succAttackers = filter null oddAttackers
--     in 
--         if 
--             length succAttackers /= length ps 
--             then [] 
--             else  
--                 concat $ evenLayer lang pMap <$> concat oddAttackers

-- queryNextLayerRebut :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
-- queryNextLayerRebut lang pMap p  = 
--     let 
--         conjunctiveRules = concat p 
--         conjunctiveConcs = L.conC <$> conjunctiveRules 
--     in queryNextLayerAttack lang pMap conjunctiveRules <$> conjunctiveConcs

-- -- | TODOs: 
-- -- What if to separate Equifinal Paths that disjunctively support `neg l`
-- -- However one of them has a circle ? 
-- -- 1. Change the name to undercut
-- queryNextLayerUndercut :: L.Language -> L.Literal -> L.EquifinalPaths
-- queryNextLayerUndercut lang l = equifinalPathForQuery lang $ M.neg l 

-- -- | `conC` is an conclusion from upper layer 
-- -- 1. get the path to `conC` (PathC), there is only one path, because it is a section of one equifinal path. 
-- -- 2. get equifinal paths to `neg conC`
-- -- 3. filter the paths of `neg conC` , select these can defeat (PathC).
-- -- returns: Arguments (Equifinal Paths) that successfully defeat `conC`.
-- -- TODOs: 
-- -- 1. Change the name : Attack in this case is defead as rebut here 
-- queryNextLayerAttack :: L.Language -> L.PreferenceMap -> L.Language -> L.Literal -> L.EquifinalPaths 
-- queryNextLayerAttack lang pMap pathRuls conC = 
--     let
--         argPath = getArgPath pathRuls conC 
--         qConc = M.neg conC 
--         attackPaths = equifinalPathForQuery lang qConc 
--         attackerPaths = filter  (`defeat` argPath) attackPaths
--     in attackerPaths 


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