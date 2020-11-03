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




-- | TODOs: env needed
-- work together with Argumentation.undercutting
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

-- | This is the same as described in pseudocode, but different from Definition 5    

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

-- | TODOs: 
-- 1. complexity of these read monad 
-- 2. analysis the necessity of using these monad contraint, check haskell report . 
-- 3. all corresponding examples

-- These are functions that relies on env (Language)
closure :: 
    ( MonadReader env m
    , Has L.Language env 
    , Has L.StrictRules env 
    , MonadIO m
    )=> [L.Literal] -> m [L.Literal]
closure p = do 
    l <- grab @L.Language 
    sRules <- L.getStrictRules <$> grab @L.StrictRules
    let 
        heads = L.conC <$> sRules 
        bodys = concat $ L.body <$> sRules 
        closurePS = M.rmdups $ heads ++ bodys ++ p 
    pure closurePS
    
-- | find all Rules whose conclusion (head) are the given literal .     
--  This actually compute the support path of a given `Literal` .
-- Function AL is running on Argumentation space. 
-- This funciton is running on Language Space.
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

rsForLiteral :: 
            ( MonadReader env m 
            , UseRuleOnly env 
            , MonadIO m 
            ) => L.Literal -> m L.Language
rsForLiteral l = do 
    rules4l <- rulesForLiteral l 
    asg rules4l rules4l 
    where 
        asg initRL endRL = do 
                rebutting <- M.rmdups . concat <$> forM ( M.neg . L.conC <$> initRL) rulesForLiteral
                undercutting <- concat <$> forM (M.neg <$>  rebutting ++ initRL) ruleAsConc
                oneMoreStepAL <- M.rmdups . concat 
                                <$> forM 
                                    (initRL ++ rebutting ++ undercutting) 
                                    rulesForLiteral
                let all = M.rmdups $ initRL ++ oneMoreStepAL ++ undercutting ++ rebutting
                if all == endRL 
                    then pure all
                    else asg all all 




-- 
-- | Once dataset has been converted to Landspace
-- It would be not possible to has rules with no rule body.
validLanguageSpace
    :: L.Language
    -> Either L.Language L.Language
validLanguageSpace = undefined