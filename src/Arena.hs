{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where

import Language 
import MetaDefinition 
import LAParser
import Argumentation
-- | Examples before Chapter 3

a,b,c,t,m :: Literal 
a = Atom "a" 
b = Atom "b"
c = Atom "c"
t = Atom "t"
m = Atom "m"

-- | TODOs
-- class Negation need dependencies information otherwise there always need
-- explicit declariation about the type of function neg
r1,r2,r3,r4,r5,r6,r7 :: Literal 
r1 = Rule "r1" [] S  c
r2 = Rule "r2" [] S  t
r3 = Rule "r3" [] S  m
r4 = Rule "r4" [] S  (neg r7)
r5 = Rule "r5" [] D  a
r6 = Rule "r6" [a] D  (neg b)   
-- r6 = Rule "r6" [] D  c      -- r6:=>c
r7 = Rule "r7" [c,t] D  b

atoms :: [Literal]
atoms = [a,b,c,t,m]

ruleS :: [Literal]
ruleS = [r1,r2,r3,r4]

ruleD :: [Literal]
ruleD = [r5,r6,r7]

--------- demo in paper ---------
demoPoolSize :: Integer
demoPoolSize = 100

demoArgumentNames :: [String]
demoArgumentNames = 
    let 
        ind = show <$> [1..demoPoolSize]
        h = repeat "A"
    in zipWith (++) h ind

demoLiterals :: [Literal]
demoLiterals = atoms ++ ruleS ++ ruleD 

demoArguments :: ArgumentationSpace
demoArguments = parsBasicArgument demoLiterals demoArgumentNames

demoPrefer1 :: Prefer
demoPrefer1 = Prefer (demoArguments !! 1) (head demoArguments ) 
demoPrefer2 :: Prefer
demoPrefer2 = Prefer (demoArguments !! 2) (demoArguments !! 1)
demoPreferSpace :: [Prefer]
demoPreferSpace = [demoPrefer1, demoPrefer2]

a1,a2,a3,a4,a5,a6,a7 :: Argumentation
a7 = head demoArguments 
a6 = head . tail $ demoArguments
a5 = demoArguments !! 2 
a4 = demoArguments !! 3 
a3 = demoArguments !! 4 
a2 = demoArguments !! 5 
a1 = demoArguments !! 6 

-- | TODOs:
-- Is this Literal data type really necessary ?
-- language :: [Literal]
-- language = l
--     where
--         rList = Literal <$> ruleS ++ ruleD
--         aList = Literal <$> atoms
--         l = rList ++ aList
-- 
-- isApplicable :: Rule -> [Name] -> Bool
-- isApplicable (Rule _ _ body _) literals =
--     let
--         bs = literal <$> body
--     in and [b `elem` literals | b <- bs]
-- 
-- data Argument = Argument 
--     { argImp :: Imp 
--     , argConc :: Literal 
--     , argBody :: [Argument]
--     , argName :: Name 
--     }
-- 
-- subArguments :: Argument -> [Argument]
-- subArguments = undefined 
-- 