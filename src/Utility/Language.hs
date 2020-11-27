{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Utility.Language 
    ( LanguageContext (..)
    , isApplicable
    , isConsistent
    , isRebutting
    , isUndercutting
    , isAttack
    , isPreferable
    , supportPathes
    , handleRule
    )where

import Control.Monad.Reader ( MonadIO, MonadReader, forM )
import           Env                  (App, Has (..), UseRuleOnly, grab)
import qualified Space.Language       as L
import qualified Space.Meta           as M (Negation(..), Imp(..), Name, rmdups)


-- |A strict rule is applicable with respect to a set of literals:
-- Names of the body of `arg1` is subet of Names of `arg2`
isApplicable :: L.Literal -> [L.Literal] -> Bool
isApplicable (L.Rule _ body _ _) ls =
    let
        s1 = L.name <$> body
        s2 = L.name <$> ls
    in and [ s `elem` s2 | s <- s1]
isApplicable (L.Atom _) _ = False

isConsistent :: L.Language -> Bool 
isConsistent literals = not . or $ auxiFunc literals []
    where 
        auxiFunc [_] acc = acc 
        auxiFunc (l:ls) acc = auxiFunc ls $ (M.negation l <$> ls)  ++ acc 

-- |TODO: Undermining need to be unified with rebutting 
-- BC algorithm sub argument is not included 
isRebutting :: L.Literal -> L.Literal -> Bool
isRebutting a b =
    let
        concA = L.conC a 
        concB = L.conC b
        isRebutting = M.negation concA concB
    in 
        isRebutting 
        && 
        M.D == L.imp b 

-- | `a` undercut `b` when 
-- 1. conclusion of `a` is the negation of argument `b`
-- 2. Toprule of `b` is defeasible rule `D`
-- TODO: now, undercut on Atom is allowed and will return False, 
-- 1. would this function cover all undercut situation described in paper? 
-- 2. Would this function cover situation other than described in paper ? for ex, Atom !?
isUndercutting :: L.Literal -> L.Literal ->  Bool
isUndercutting a b = 
    let 
        concA = L.conC a 
    in 
        M.negation concA b 
        && 
        L.imp b == M.D

isAttack :: L.Literal -> L.Literal -> Bool 
isAttack a b = a `isRebutting` b || a `isUndercutting` b

isPreferable :: L.PreferenceSpace -> L.Literal -> L.Literal ->  Bool 
isPreferable preferSpace la lb = L.Preference la lb `elem` preferSpace

class Monad m => LanguageContext m where
    langMatchRuleFromAnony :: L.AnonyRule -> m L.Language
    langRuleAsConc :: L.Literal -> m L.Language
    langClosure :: L.Language -> m L.Language
    langAL :: L.Literal -> m L.Language 
    langASG :: L.Literal -> m L.Language 

-- | LanguageContext are functions that relies on Language (a set of Literal) implicitly.   \\ 
-- Detailed complexity in functions
instance LanguageContext App where
    langMatchRuleFromAnony = retriveRuleFromAnony
    langRuleAsConc = ruleAsConc
    langClosure = closure
    langAL = rulesForLiteral
    langASG = rsForLiteral

-- |  Work for Argumentation.undercutting : O(n)     
-- Toprule function of Argumenation space returns rule with no name (Anonymonus Rule)     
-- There fore we need to find out if this rule is a member of our `Language`    
-- Depends on `Language` of the `env`. 
retriveRuleFromAnony::
    ( MonadReader env m
    , Has L.Language env
    , MonadIO m )
    => L.AnonyRule -> m L.Language
retriveRuleFromAnony ar = do
    language <- grab @L.Language
    pure $ auxi language ar
    where
        auxi (l:ls) ar =
            case l of
                r@L.Rule{} ->
                    if ar == L.AnonyRule r
                        then [r]
                        else auxi ls ar
                L.Atom{} -> auxi ls ar

-- | TR functions. O(n)    
-- Given a rule `r`::`L.Literal`,   
-- Returns all  `rules`::`L.Language` that conclude `r`.
ruleAsConc :: 
    ( MonadReader env m 
    , UseRuleOnly env
    , MonadIO m 
    )
    => L.Literal -> m L.Language 
ruleAsConc r = do 
    sRule <- L.getStrictRules <$> grab @L.StrictRules
    dRule <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    pure $ auxiFunc r (sRule ++ dRule) []
        where
            auxiFunc _ [] acc = acc
            auxiFunc (L.Atom _) _ _ = []
            auxiFunc r@(L.Rule n _ _ _) (l:ls) acc =
                if n == (L.name . L.conC $ l)
                    then auxiFunc r ls (l:acc)
                    else auxiFunc r ls acc

-- | Construct closure of a set of Literal `p`. O(n^2)    
-- Based on inital `p`     
-- Find all `rules` isApplicable on `p` and get corresponding conclusion `Cs`    
-- Add `rules`, `Cs` in `p` to get `newP`     
-- run closure `newP` again.     
-- Stop when `p` == `newP`    
closure :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m
    )=> [L.Literal] -> m [L.Literal]
closure p = do 
    sRules <- L.getStrictRules <$> grab @L.StrictRules
    let 
        heads = L.conC <$> sRules 
        bodys = concat $ L.body <$> sRules 
        closurePS = M.rmdups $ heads ++ bodys ++ p 
    pure closurePS
    
-- | Recursively find all Rules whose conclusion (head) are the given literal. O(n^2)\\
-- And find all rules whose conclusion are body's of these rules found above.\\\
-- And continue above step.     
-- This actually compute the support path of a given `Literal` .\\
-- It is different from GRI, because support path in GRI only compute support path for rule.\\\
-- It is different from `Definition 5`, because `Definition 5` has no recursive operation. \\
-- It is the same as described in the pseudo-code of `AL`.\\
-- This is the core of implementation of `BC.funcAL`.
rulesForLiteral ::
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.Language
rulesForLiteral l = do 
    sRules <- L.getStrictRules <$> grab @L.StrictRules
    dRules <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    pure $  M.rmdups $ accBodys [l] (sRules ++ dRules) [] []
    where
        accBodys [] _ _ acc = acc
        accBodys (ll:ls) lSpace seen acc =
            if
                ll `elem` seen then accBodys ls lSpace seen acc
                else
                    let tmpR = [ r | r <-lSpace , L.conC r == ll] 
                        tmpLit = concat (L.body <$> tmpR) ++ ls
                    in accBodys tmpLit lSpace (ll : seen) (tmpR ++ acc)

-- | Line 1-16 of ASG  
-- `rebutting` relies on `AL` to get all support path \\
-- `undercutting` relies on `neg` of previous result to get all negative connected rules.\\
-- `OneMoreStepAL` relies on `AL` to get all connected rules.\\
-- Sum these together and repeat again, untill no more rule are included.
rsForLiteral :: 
            ( MonadReader env m 
            , UseRuleOnly env 
            , MonadIO m 
            ) => L.Literal -> m L.Language
rsForLiteral l = do 
    rules4l <- rulesForLiteral l 
    asg rules4l 
    where 
        asg initRL = do 
                rebutting <- M.rmdups . concat <$> forM ( M.neg . L.conC <$> initRL) rulesForLiteral
                undercutting <- concat <$> forM (M.neg <$>  rebutting ++ initRL) ruleAsConc
                oneMoreStepAL <- M.rmdups . concat 
                                <$> forM 
                                    (initRL ++ rebutting ++ undercutting) 
                                    rulesForLiteral
                let all = M.rmdups $ initRL ++ oneMoreStepAL ++ undercutting ++ rebutting
                if all == initRL 
                    then pure all
                    else asg all 


-- | [[a],[b1,b2,b3],[c2,c3,c4]...]
-- body of `a` supported by b1,b2,b3
-- bodies of b1,b2,b3, supported by c2,c3,c4
type Path = [L.Language]

-- supportPaths :: 
--     ( MonadReader env m 
--     , UseRuleOnly env 
--     , MonadIO m 
--     ) => L.Literal -> m [Path]
-- supportPaths l = do 
--     sRules <- L.getStrictRules <$> grab @L.StrictRules
--     dRules <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
--     case scanLan4Paths l (sRules++dRules) of 
--         Nothing -> error "Target argument does not reach the ground"
--         Just a -> pure a 
--     where
--         scanLan4Paths :: L.Literal -> L.Language -> Maybe L.Language 
--         scanLan4Paths l lang =  
--             let 
--                 supportRules = [ r | r <- lang , L.conC r == l]
--             in undefined 

-- p lang sr = 
--         let
--             paral = handleRule lang sr 
--             acc = 
--                 do 
--                     s <-  sr 
--                     p <- paral 
--                     pure $ s : p 
--         in 
--             concat $ p lang acc <$> paral 

-- | TODOs: why DOES this work ? ? 
-- what is concat here ? I write this code all by myself and .....
supportPathes :: L.Language -> L.Language -> [[L.Language]]
supportPathes lang sRules = 
    let bodies = concat $ L.body <$> sRules
    in 
        if null bodies
            then [[sRules]]
        else 
            let branches = handleRule lang bodies  
            in
                do 
                    b <-  concat $ supportPathes lang <$> branches 
                    pure $ sRules : b

-- | branch (Language) is a section of path ([Language])
-- from a branch to sub level parallel branches [Language]. 
handleRule :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> [[L.Literal]]
handleRule lang subBodies= 
    let subLevel = concludedBy lang <$> subBodies
    in foldr acc [[]] subLevel 
    where 
        acc :: [a] -> [[a]] -> [[a]] 
        acc l ls = do 
                e <- l 
                a <- ls 
                pure $  e:a 
        concludedBy :: L.Language -> L.Literal -> L.Language
        concludedBy lang l = [r | r <- lang , L.conC r == l]



-- | Once dataset has been converted to Landspace
-- It would be not possible to has rules with no rule body.
-- 1. to check if there are loop support.
-- 2. to check if every conclusion has ground support. 
-- 3. Others:
validLanguageSpace
    :: L.Language
    -> Either L.Language L.Language
validLanguageSpace = undefined


scanOrderedSupportPath :: [[L.Literal]] -> ([L.Literal],Bool)
scanOrderedSupportPath = undefined 

getOrderedSupportPath :: L.Language -> M.Name -> [[L.Literal]]
getOrderedSupportPath = undefined 

tracer :: L.LanguageMap -> [L.Literal] -> Bool
tracer = undefined 

