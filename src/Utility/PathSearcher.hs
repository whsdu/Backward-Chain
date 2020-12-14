{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}
module Utility.PathSearcher where 

import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)
import Data.List (sort, reverse)

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
    pure $ sort $ concat rs 


{- Backward Chaining Search
Given: 
1. The rule level graph representation (the List of Rules).
2. The prefernceMap that can be used to compute the preference relation between two arugment. 
find the 
-}

-- eveLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> 
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
        newLang = removeSeenPath p lang 
        defenders = oddLayer' newLang pMap <$> attackGroup
        succDefeaders = filter (not . null) defenders
    in if length succDefeaders == length attackGroup 
        then [p] : concat ( concat succDefeaders)
        else evenLayer' lang pMap ps 
evenLayer' _ _ [] = []

-- | every path in this list of equifinal paths should be defeated 
-- If all these paths are defeated then return these path together with the next layer defeaders path 
-- If not all these path are dfeated then return [].
-- oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [[L.EquifinalPaths]]
oddLayer' lang pMap ps = 
    let 
        attackGroups = [ (p, queryPathAttackers lang pMap p) | p <- ps , (not . null) (queryPathAttackers lang pMap p)]
    in if length attackGroups /=  length ps 
        then [] 
        else 
            let 
                seen = [ p | p <- ps , (not . null) (queryPathAttackers lang pMap p)]
                newLang = removeSeenPath (concat seen) lang 
            -- in checkAttackGroup newLang pMap <$> attackGroups
                tt = checkAttackGroup newLang pMap <$> attackGroups
            in 
                if [] `notElem` tt then tt else [] 
    where 
        checkAttackGroup :: L.Language -> L.PreferenceMap -> (L.Path ,[L.EquifinalPaths]) -> [L.EquifinalPaths]
        checkAttackGroup lang fMap (path, group) = 
            let 
                defenderGroup = concat (evenLayer' lang fMap <$> group)
            in 
                if not . null $ defenderGroup  then [path] : defenderGroup else [] 

removeSeenPath :: L.Path -> L.Language -> L.Language
removeSeenPath path lang = 
    let 
        seen = concat path 
    in [l | l <-lang , l `notElem` seen]

sortEquifinalPaths :: L.EquifinalPaths -> L.EquifinalPaths 
sortEquifinalPaths paths = 
    let 
        pathLengths = length <$> paths 
        sortedLength = sort pathLengths
    in [p | l <- sortedLength, p <- paths, length p == l]

queryPathAttackers :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
queryPathAttackers lang pMap path = 
    let 
        nextLayerRebutting = queryPathRebuts' lang pMap path
        nextLayerUndercutting = queryPathUndercut' lang path 
    in filter ( not .null ) (nextLayerRebutting ++ nextLayerUndercutting)

-- | TODOs: 
-- What if to separate Equifinal Paths that disjunctively support `neg l`
-- However one of them has a circle ? 
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
    in queryConcRebuts' lang pMap p <$> conjunctiveConcs

-- | Given conclusion c
-- Get all Equifinal paths of neg c
-- select paths that defeat path of c.
-- TODO: 
-- Ordering functions is hard coded here. 
queryConcRebuts' :: L.Language -> L.PreferenceMap -> L.Path -> L.Literal -> L.EquifinalPaths 
queryConcRebuts' lang pMap pathRuls conC = 
    let
        qConc = M.neg conC 
        argPath = head $ equifinalPathForQuery' (concat pathRuls) conC  -- This part do not need to lift to monad level
        attackPaths = equifinalPathForQuery' lang qConc 
    in [p | p <- attackPaths, O.weakestLink pMap O.dem p argPath] -- This is problematic to convert to monad level 

{-
---- | below is the original implementation of Equifinal Paths
-}

-- | Sorted by depth of the path. 
-- Short path came first 


equifinalPathForQuery' :: L.Language -> L.Literal -> L.EquifinalPaths
equifinalPathForQuery' lang conC= 
    let 
        dos = (:[]) <$> concludedBy' lang conC
    in sortEquifinalPaths . concat $ equifinalPaths' lang <$> dos

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