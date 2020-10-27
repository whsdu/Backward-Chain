module LAParser where 

import Language ( Literal(Rule), literal ) 
import Argumentation
    ( ArgumentationSpace,
      Argumentation(Argumentation, argName, argBody, argConc, argImp) ) 
import Control.Monad (guard)



-- | check if same literal in data source is valid 
-- 1. same atom literal has different Imp  (-> and ~>)
-- 2. other properties need to satisfied.
parsLanguageCheck :: [Literal] -> [Literal]
parsLanguageCheck = undefined 

parsBasicArgument :: [Literal] -> [String] -> ArgumentationSpace
parsBasicArgument ls aNames= knowledge'2'argument ls aNames []
    where 
        knowledge'2'argument :: [Literal] -> [String] -> ArgumentationSpace -> ArgumentationSpace
        knowledge'2'argument [] _ al = al 
        knowledge'2'argument (r:rs) names@(n:ns) al = 
            case r of 
              (Rule _ [] imp c) -> 
                  let newArgument = Argumentation 
                                      { argImp = imp
                                      , argConc = c 
                                      , argBody = []
                                      , argName = n
                                      }
                  in knowledge'2'argument rs ns (newArgument:al)
              (Rule _ bs imp c) -> 
                  let 
                      ruleLiteralBodyNames = literal <$> bs 
                      ruleLiteralBodyArgs= do 
                          rname <- ruleLiteralBodyNames 
                          a <- al 
                          guard $ rname == literal (argConc a)
                          return  a 
                  in    
                    if length ruleLiteralBodyNames <= length ruleLiteralBodyArgs
                          then 
                              let newArgument = Argumentation
                                                  { argImp =  imp
                                                  , argConc = c
                                                  , argBody = ruleLiteralBodyArgs
                                                  , argName = n 
                                                  }
                                in knowledge'2'argument rs ns (newArgument:al)
                   else knowledge'2'argument rs names al 
              _ -> knowledge'2'argument rs names al 


-- | How the preference was computed

-- | types being used for testing and demo examples

