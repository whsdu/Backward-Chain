{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where

import           AtomSpace       (Atom (..))
import           RuleSpace       (Rule (..))

import           Data.List.Split (splitOn)
import           MetaDefinition  (Imp (..), Negation (..), literal)

instance Negation Atom Rule where
    negation _ _ = False
    neg _ = undefined

instance Negation Rule Atom where
    negation _ _ = False
    neg _ = undefined

instance Negation Rule Rule where
    negation l1 l2
        |   literal l1 == literal l2 = False
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True
        |   otherwise = False

    neg l1
        |  head (splitOn "_" (literal l1)) == "_" =
            let nName = last (splitOn "_" (literal l1))
            in l1{ruleName = nName}
        | otherwise =
            let nName = "_" ++ literal l1
            in l1{ruleName = nName}


instance Negation Atom Atom where
    negation l1 l2
        |   literal l1 == literal l2 = False
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True
        |   otherwise = False
    neg l1
        |  head (splitOn "_" (literal l1)) == "_" =
            let nName = last (splitOn "_" (literal l1))
            in l1{atomName = nName}
        | otherwise =
            let nName = "_" ++ literal l1
            in l1{atomName = nName}

-- | Examples before Chapter 3

a,b,c,t,m :: Atom
a = Atom "a"
b = Atom "b"
c = Atom "c"
t = Atom "t"
m = Atom "m"

-- | TODOs
-- class Negation need dependencies information otherwise there always need
-- explicit declariation about the type of function neg
r1,r2,r3,r4,r5,r6,r7 :: Rule
r1 = Rule {ruleName="r1",getImp=S, getBody=[],getHead= c}
r2 = Rule {ruleName="r2",getImp=S, getBody=[],getHead= t}
r3 = Rule {ruleName="r1",getImp=S, getBody=[],getHead= m}
r4 = Rule {ruleName="r1",getImp=S, getBody=[],getHead= neg r7 :: Rule}
r5 = Rule {ruleName="r1",getImp=D, getBody=[],getHead= a}
r6 = Rule {ruleName="r1",getImp=D, getBody=[],getHead= c}
r7 = Rule { ruleName="r1"
            , getImp=D
            , getBody= [c,t]
            , getHead= b}
