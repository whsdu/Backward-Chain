{-# LANGUAGE GADTs #-}
module Language where

import           MetaDefinition  (Imp (..), Name, Negation (..))
import qualified Data.HashMap.Strict as Map
import Control.Monad (guard)
data Literal where
    Atom :: Name -> Literal
    Rule :: Name -> [Literal] -> Imp -> Literal -> Literal

type LanguageSpace = [Literal]
type LanguageMap = Map.HashMap Name Literal 

-- | literal is function 'Name' , introduced in last line of page 2.
-- Handle for rules to prevent other rule application is expressed in the
-- instance of Negation
literal :: Literal -> Name
literal (Rule n _ _ _) = n
literal (Atom n)       = n

ruleBody  :: Literal -> [Literal]
ruleBody (Rule _ b _ _) = b
ruleBody (Atom _)       = []

ruleImp :: Literal -> Imp
ruleImp (Rule _ _ i _) = i
ruleImp (Atom _)       = N

ruleHead :: Literal -> Literal
ruleHead (Rule _ _ _ h) = h
ruleHead a@(Atom _)     = a

instance Show Literal where
    show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
        where
            bs = unwords $ literal <$> b
            im = show i
            head = literal h
    show (Atom n) = n

instance Eq Literal where
    (==) l1 l2 = literal l1 == literal l2

-- | By default : negation a1 a2 = neg a1 == a2
instance Negation Literal where
    neg (Rule n b i h)
        |  head n == '_' =
            let nLiteral = tail n 
            in Rule nLiteral b i h
        | otherwise =
            let nLiteral = '_' : n
            in Rule nLiteral b i h
    neg (Atom n)
        |  head n  == '_' =
            let nLiteral = tail n
            in Atom nLiteral
        | otherwise =
            let nLiteral = '_' : n
            in Atom nLiteral

newtype AnonyRule = AnonyRule {unanonyRule :: Literal}

instance Eq AnonyRule where
    (==) aR1 aR2
        | ruleImp  r1 /= ruleImp r2 = False
        | ruleHead r1 /= ruleHead r2 = False
        | ruleBody r1 /= ruleBody r2 = False
        | otherwise = True
        where
            r1 = unanonyRule aR1
            r2 = unanonyRule aR2

instance Show AnonyRule where
    show ar = show $ unanonyRule ar

-- | Properties below:

isApplicable :: Literal -> [Literal] -> Bool
isApplicable (Rule _ body _ _) ls =
    let
        s1 = literal <$> body
        s2 = literal <$> ls
    in and [ s `elem` s2 | s <- s1]
isApplicable (Atom _) _ = False

-- | TODOs: env needed 
-- work together with Argumentation.undercutting 
retriveRuleFromAnon :: AnonyRule -> [Literal] ->[Literal]
retriveRuleFromAnon ar (l:ls)= 
    case l of 
        r@Rule{} -> 
            if ar == AnonyRule r 
                then [r]
                else retriveRuleFromAnon ar ls
        Atom{} -> retriveRuleFromAnon ar ls 

-- | Similar with the as parsBasicArgument 
-- line 1-10 in algorithm AL 
-- > 
-- > let al = subBodys demoLiterals b
-- > al 
-- > [t,r2: ->t,c,r1: ->c,b,r7: c t=>b]
-- > parsBasicArgument al demoArgumentNames
-- > [A3 :A2 A1=>b,A2 :->c,A1 :->t]
subBodys :: LanguageSpace -> Literal -> LanguageSpace
subBodys ls l = accBodys [l] ls [] [] 
    where 
        accBodys [] _ _ acc = acc 
        accBodys (ll:ls) lSpace seen acc = 
            if 
                ll `elem` seen then accBodys ls lSpace seen acc 
                else
                    let tmpR = [ r | r <-lSpace , ruleHead r == ll ]
                        tmpLit = concat (ruleBody <$> tmpR) ++ ls 
                    in accBodys tmpLit lSpace (ll : seen) (tmpR ++ acc)


closure :: [Literal] -> [Literal]
closure = undefined

-- | Once dataset has been converted to Landspace
-- It would be not possible to has rules with no rule body. 
validLanguageSpace 
    :: LanguageSpace 
    -> Either LanguageSpace LanguageSpace 
validLanguageSpace = undefined 

