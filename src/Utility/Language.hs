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
    , equifinalPaths
    , equifinalPathSections
    , equifinalPathForQuery
    , concludedBy
    )where

import Control.Monad.Reader ( MonadIO, MonadReader, forM )
import           Env                  (App, Has (..), UseRuleOnly, grab)
import qualified Space.Language       as L
import qualified Space.Meta           as M (Negation(..), Imp(..), Name, rmdups)
import qualified Data.HashMap.Strict as Map

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

-- | sRules conjunctively concluded some conclusion
equifinalPaths :: L.Language -> L.Language -> L.EquifinalPaths
equifinalPaths lang sRules = 
    let bodies = concat $ L.body <$> sRules
    in 
        if null bodies
            then [[sRules]]
        else 
            let equifinality = equifinalPathSections lang bodies  
            in
                do 
                    b <-  concat $ equifinalPaths lang <$> equifinality 
                    pure $ sRules : b



-- | 
-- `lang`: env rule space
-- `bodies` : bodies that concluded by lower level rules.
-- `return`: equifinal path sections from lower-level rules to input bodies.[ path1, path2, path3 ...]
equifinalPathSections :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> L.EquifinalPathSections
equifinalPathSections lang bodies =  
    let subLevel = concludedBy lang <$> bodies 
    in foldr createParallel [[]] subLevel 
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                path <- paths
                a <- ls 
                pure $  path:a 

concludedBy :: L.Language -> L.Literal -> L.Language
concludedBy lang l = [r | r <- lang , L.conC r == l]

equifinalPathForQuery :: L.Language -> L.Literal -> L.EquifinalPaths
equifinalPathForQuery lang atom = 
    let 
        dos = (:[]) <$> concludedBy lang atom 
    in concat $ equifinalPaths lang <$> dos


-- | TODO:
-- remove duplicated list of lists
-- In a 3 level list, remove duplicate list of lists , for example
{-
[ [[1],[1,2,3],[4,5]]
, [[2,3],[1,2,3]]
, [[1],[1,2,3],[4,5]]
]
-}
-- the 1st and 3rd element are the same, one should be removed
removeSndDup :: (Eq a) =>  [[[a]]] -> [[[a]]]
removeSndDup = undefined 


-- |  TODO:
-- In a 3 level list, remove elements that exists in previous lists, for example
{-
[ [[1],[1,2,3],[4,5]]
, [[2,3],[1,2,3]]
, [[1],[1,2,3,4],[4,5]]
]
-}
-- the 3rd array, 4 appears in [1,2,3,4] and [4,5], thus the 4 in [4,5] should be removed. 
removeExistingFstElem :: (Eq a) =>  [[[a]]] -> [[[a]]]
removeExistingFstElem = undefined 

-- | TODO: 
-- Convert a Path to individual Strings 
-- A String is a Chain of rules from Argument Conclusion to the fact
-- For each String, we could find at most one last defeasible rule. 
-- combine them together we have LastDefRules of a Path.
pathToChains:: L.Path -> [L.Language]
pathToChains = undefined 

-- | TODO:
-- Premises / Facts/ Grounds of a Path
pathToAxiomFacts:: L.Path -> L.Language 
pathToAxiomFacts = undefined 

pathToOrdinaryFacts:: L.Path -> L.Language 
pathToOrdinaryFacts = undefined 

chainToLastDefRule :: L.Language -> L.Literal
chainToLastDefRule = undefined 

chainToDefRules :: L.Language -> L.Language 
chainToDefRules = undefined 

isStrictPath :: L.Path -> Bool 
isStrictPath = undefined 

isFirmPath :: L.Path -> Bool 
isFirmPath = undefined 

type PreferMap = Map.HashMap L.Literal Int 
type Orderings = PreferMap -> L.Language -> L.Language -> Bool 
type OrderingLink = PreferMap -> Orderings -> L.Path -> L.Path -> Bool 

eli :: PreferMap -> L.Language -> L.Language -> Bool 
eli = undefined 

dem :: PreferMap -> L.Language -> L.Language -> Bool 
dem = undefined 

---- aboves are 1st level functions
---- now is 2nd level functions that relies on them 

lastLink :: PreferMap -> Orderings -> L.Path -> L.Path -> Bool 
lastLink pm orderings pathA pathB 
    | null ldrA && null ldrB = orderings pm (axiA ++ ordiA) (axiB ++ ordiB)
    | otherwise = orderings pm ldrA ldrB
  where 
    ldrA :: L.Language
    ldrA = chainToLastDefRule <$> pathToChains pathA
    ldrB :: L.Language
    ldrB = chainToLastDefRule <$> pathToChains pathB
    axiA :: L.Language
    axiA = pathToAxiomFacts pathA 
    axiB :: L.Language
    axiB = pathToAxiomFacts pathB
    ordiA :: L.Language
    ordiA = pathToOrdinaryFacts pathA 
    ordiB :: L.Language
    ordiB = pathToOrdinaryFacts pathB

weakestLink :: PreferMap -> Orderings -> L.Path -> L.Path -> Bool 
weakestLink pm orderings pathA pathB 
    | isStrictPath pathA && isStrictPath pathB = orderings pm ordiA ordiB 
    | isFirmPath pathA && isFirmPath pathB = orderings pm drA drB
    | otherwise = orderings pm ordiA ordiB && orderings pm drA drB 
  where 
    drA :: L.Language
    drA = concat $ chainToDefRules <$> pathToChains pathA
    drB :: L.Language
    drB = concat $ chainToDefRules <$> pathToChains pathB
    ordiA :: L.Language
    ordiA = pathToOrdinaryFacts pathA 
    ordiB :: L.Language
    ordiB = pathToOrdinaryFacts pathB

-- | the successful path and related attacker paths
-- What we want from the query ? 
-- How to represent the Def relation. 
p :: L.EquifinalPaths -> OrderingLink -> Orderings 
p equiPaths oL = undefined 

-- | TODO:
-- For Last- link 

-- |TODO:
-- For weakest -link 
-- 1. Path is strict 
-- 2. Path is firm 
-- ect...

-- TODO: 
-- 1. map a path to a list of LastDefRules 
-- 2. map a path to a list of facts.
-- 3. necessary for weakest-link
--      3.1. check if a Path strict (no defeasible rules involved).
--      3.2. check if a Path firm (relies on only axiom premises)
----------------------------------------------
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

