{-# LANGUAGE GADTs #-}
module Argumentation 
    ( Argumentation(..)
    , Prefer(..)
    , ArgumentationSpace
    , PreferrenceSpace 
    , preferable
    , subArgument
    , topRule
    , rebutting
    , undercutting
    , defeats
    )where 

import Language 
import MetaDefinition 

data Argumentation = Argumentation 
    { argName :: Name 
    , argBody :: [Argumentation]
    , argImp :: Imp
    , argConc :: Literal
    }

data Prefer where
    Prefer :: Argumentation -> Argumentation -> Prefer  
    
type ArgumentationSpace = [Argumentation]
type PreferrenceSpace = [Prefer]

instance Show Argumentation where 
    show (Argumentation n b i c) = 
        n ++ " :" ++ body ++ imp ++ head 
        where 
            body = unwords $ argName <$> b 
            imp = show i 
            head = literal c

instance Eq Argumentation where 
    (==) a1 a2 = argName a1 == argName a2 


instance Show Prefer where 
    show (Prefer a1 a2) = argName a1 ++ " > " ++ argName a2

instance Eq Prefer where 
    (==) (Prefer pr11 pr12) (Prefer pr21 pr22)
        | pr11 /= pr21 = False 
        | pr12 /= pr22 = False 
        | otherwise = True

-- | TODOs: PreferenceSpace should be initialized in env
preferable :: PreferrenceSpace ->  Argumentation -> Argumentation -> Bool 
preferable pf a1 a2 = 
    let 
        candiPrefer = Prefer a1 a2 
    in or $ (==) candiPrefer <$> pf 

-- | Utility functions:
subArgument :: Argumentation -> [Argumentation] 
subArgument a@(Argumentation _ [] _ _) = [a]
subArgument a@(Argumentation _ bs _ _) = 
    let 
        h = [a] 
        t = concat $ subArgument <$> bs 
    in h ++ t

topRule :: Argumentation -> AnonyRule 
topRule (Argumentation _ bs i c) = 
    let 
        ruleBody = argConc <$> bs 
        anonyRule = Rule "" ruleBody i c 
    in AnonyRule anonyRule 

rebutting :: Argumentation -> Argumentation -> Bool 
rebutting a b = 
    let 
        negConcA = neg . argConc $ a 
        bPrime = [b' | b' <- subArgument b, argConc b' == negConcA]
    in 
        not (null bPrime) 
        && 
        D `elem` (argImp <$> bPrime)

-- | TODOs: env need includes Language Space and Prefererence
-- 1.  Language Space will be expanded as required .
undercutting :: [Literal] -> Argumentation -> Argumentation -> Bool 
undercutting ls a b = 
    let 
        concA = argConc a 
        negNameTRB = neg <$> getRuleLiteral (topRule b) ls 
    in concA `elem` negNameTRB

defeats :: [Literal] -> PreferrenceSpace -> Argumentation -> Argumentation  -> Bool 
defeats ls ps a b = 
    let 
        isRebutting = a `rebutting` b 
        isUndercutting = undercutting ls a b 
        isPreferable = preferable ps a b 
    in (isUndercutting || isRebutting) && isPreferable
