{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where 

import qualified AtomSpace as A (Atom(..))
import qualified RuleSpace as R (Rule(..))

import Data.List.Split (splitOn)
import MetaDefinition ( Negation(..),literal )

instance Negation A.Atom R.Rule where 
    negation _ _ = False 
    neg _ = undefined 
    
instance Negation R.Rule A.Atom where 
    negation _ _ = False 
    neg _ = undefined 

instance Negation R.Rule R.Rule where 
    negation l1 l2 
        |   literal l1 == literal l2 = False 
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True 
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True 
        |   otherwise = False 

    neg l1 
        |  head (splitOn "_" (literal l1)) == "_" = 
            let nName = last (splitOn "_" (literal l1))
            in l1{R.getName=nName}
        | otherwise = 
            let nName = "_" ++ literal l1 
            in l1{R.getName=nName}


instance Negation A.Atom A.Atom where 
    negation l1 l2 
        |   literal l1 == literal l2 = False 
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True 
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True 
        |   otherwise = False 
    neg l1 
        |  head (splitOn "_" (literal l1)) == "_" = 
            let nName = last (splitOn "_" (literal l1))
            in l1{A.getName=nName}
        | otherwise = 
            let nName = "_" ++ literal l1 
            in l1{A.getName=nName}

