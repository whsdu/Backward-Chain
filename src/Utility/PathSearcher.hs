{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}
module Utility.PathSearcher where 

import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)

import qualified Space.Language as L 
import Env 
import qualified Utility.Language as LU 


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



---- | below is the original implementation 

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