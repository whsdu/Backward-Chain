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
    )where

import           Control.Monad.Reader
import           Env                  (App, Has (..), UseRuleOnly, grab)
import qualified Space.Language       as L
import qualified Space.Meta           as M (Negation(..), Imp(..), rmdups)


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
            
isUndercutting :: L.Literal -> L.Literal ->  Bool
isUndercutting a b = 
    let 
        concA = L.conC a 
    in M.negation concA b

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
-- And find all rules whos conclusion are bodys of these rules found above.\\\
-- And continue above step.     
-- This actually compute the support path of a given `Literal` .\\
-- It is different from GRI, because support path in GRI only compute support path for rule.\\\
-- It is different from `Definition 5`, because `Definition 5` has no recursive operation. \\
-- It is the same as descrbied in the pseudo-code of `AL`.\\
-- This is the core of implemenation of `BC.funcAL`.
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

-- | Once dataset has been converted to Landspace
-- It would be not possible to has rules with no rule body.
validLanguageSpace
    :: L.Language
    -> Either L.Language L.Language
validLanguageSpace = undefined