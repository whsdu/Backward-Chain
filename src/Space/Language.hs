{-# LANGUAGE GADTs #-}
module Space.Language 
    ( Literal (..)
    , Language 
    , LanguageMap
    , AnonyRule(..)
    , StrictRules (..)
    , DefeasibleRules(..)
    , name 
    , body
    , imp
    , conC
    , eqLang
    )where

import Space.Meta (Name, Imp(..), Negation(..))
import qualified Data.HashMap.Strict as Map
import qualified GHC.List as GHC (head)


data Literal where
    Atom :: Name -> Literal
    Rule :: Name -> [Literal] -> Imp -> Literal -> Literal

type Language = [Literal]
type LanguageMap = Map.HashMap Name Literal 
newtype StrictRules = StrictRules {getStrictRules :: Language}
newtype DefeasibleRules = DefeasibleRules {getDefeasibleRules :: Language}

-- | literal is function 'Name' , introduced in last line of page 2.
-- Handle for rules to prevent other rule application is expressed in the
-- instance of Negation
name :: Literal -> Name
name (Rule n _ _ _) = n
name (Atom n)       = n

body  :: Literal -> [Literal]
body (Rule _ b _ _) = b
body (Atom _)       = []

imp :: Literal -> Imp
imp (Rule _ _ i _) = i
imp (Atom _)       = N

conC :: Literal -> Literal
conC (Rule _ _ _ h) = h
conC a@(Atom _)     = a

eqLang :: Language -> Language -> Bool 
eqLang al bl = isElemB && isElemA 
    where 
        isElemB = and [ a `elem` bl | a <- al ]
        isElemA = and [ b `elem` al | b <- bl ]
instance Show Literal where
    show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
        where
            bs = unwords $ name <$> b
            im = show i
            head = name h
    show (Atom n) = n

instance Eq Literal where
    (==) l1 l2 = name l1 == name l2

instance Ord Literal where 
    compare l1 l2 = compare (name l1) (name l2 )

-- | By default : negation a1 a2 = neg a1 == a2
instance Negation Literal where
    neg (Rule n b i h)
        |  GHC.head n == '!' =
            let nLiteral = tail n 
            in Rule nLiteral b i h
        | otherwise =
            let nLiteral = '!' : n
            in Rule nLiteral b i h
    neg (Atom n)
        |  GHC.head n  == '!' =
            let nLiteral = tail n
            in Atom nLiteral
        | otherwise =
            let nLiteral = '!' : n
            in Atom nLiteral

newtype AnonyRule = AnonyRule {unanonyRule :: Literal}

instance Eq AnonyRule where
    (==) aR1 aR2
        | imp  r1 /= imp r2 = False
        | conC r1 /= conC r2 = False
        | body r1 /= body r2 = False
        | otherwise = True
        where
            r1 = unanonyRule aR1
            r2 = unanonyRule aR2

instance Show AnonyRule where
    show ar = show $ unanonyRule ar 