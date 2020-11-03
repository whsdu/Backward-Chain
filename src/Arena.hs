{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where

import           qualified Space.Argumentation as A 
import           qualified Space.Language   as L 
import           qualified Parser.LanguageParser as PL
import           qualified Space.Meta as M 
import           Env(Env(..))
-- | Examples before Chapter 3

a,b,c,t,m :: L.Literal
a = L.Atom "a"
b = L.Atom "b"
c = L.Atom "c"
t = L.Atom "t"
m = L.Atom "m"

-- | TODOs
-- class Negation need dependencies information otherwise there always need
-- explicit declariation about the type of function neg
r1,r2,r3,r4,r5,r6,r7 :: L.Literal
r1 = L.Rule "r1" [] M.S  c
r2 = L.Rule "r2" [] M.S  t
r3 = L.Rule "r3" [] M.S  m
r4 = L.Rule "r4" [] M.S  (M.neg r7)
r5 = L.Rule "r5" [] M.D  a
r6 = L.Rule "r6" [a] M.D  (M.neg b)
-- r6 = Rule "r6" [] D  c      -- r6:=>c
r7 = L.Rule "r7" [c,t] M.D  b

lr1 = L.Rule "lr1" [lr2] M.D lr1 
lr2 = L.Rule "lr2" [lr1] M.D lr2

atoms :: [L.Literal]
atoms = [a,b,c,t,m]

ruleS :: [L.Literal]
ruleS = [r1,r2,r3,r4]

ruleD :: [L.Literal]
ruleD = [r5,r6,r7]

rChain :: a
rChain = undefined
--------- demo in paper ---------
demoPoolSize :: Integer
demoPoolSize = 100

demoArgumentNames :: [String]
demoArgumentNames =
    let
        ind = show <$> [1..demoPoolSize]
        h = repeat "A"
    in zipWith (++) h ind

demoLanguage :: L.Language
demoLanguage = atoms ++ ruleS ++ ruleD

demoArguments :: A.ArgumentationSpace
demoArguments = PL.parsBasicArgument demoLanguage demoArgumentNames

demoPrefer1 :: L.Preference
demoPrefer1 = L.Preference r7 r6
demoPrefer2 :: L.Preference
demoPrefer2 = L.Preference r4 r7
demoPreferSpace :: [L.Preference]
demoPreferSpace = [demoPrefer1, demoPrefer2]


paperEnv :: Env
paperEnv = Env demoLanguage (L.StrictRules [r1,r2,r3,r4]) (L.DefeasibleRules [r5,r6,r7]) demoArguments demoPreferSpace

a1,a2,a3,a4,a5,a6,a7 :: A.Argumentation
a7 = head demoArguments
a6 = head . tail $ demoArguments
a5 = demoArguments !! 2
a4 = demoArguments !! 3
a3 = demoArguments !! 4
a2 = demoArguments !! 5
a1 = demoArguments !! 6


-- | Benchmark test
datasetRoot :: FilePath
datasetRoot = "./Example/Chains/"
defeasibleRoot :: FilePath
defeasibleRoot = datasetRoot ++ "Defeasbile"
strictRoot :: FilePath
strictRoot= datasetRoot ++ "Strict"
