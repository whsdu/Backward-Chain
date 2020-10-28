module BC where 

import Language 
import Argumentation 
import LAParser 
import MetaDefinition

funcAL :: Language -> [Name] -> Literal -> ArgumentationSpace
funcAL language argNames l = 
    let 
        prod = subBodys language l 
    in parsBasicArgument prod argNames 

funcASG :: [Literal] -> Literal -> [Literal]
funcASG language l = 
    let 
        al = subBodys language l 
    in computeASG language al al 
    where 
        computeASG lag initAL endAL = 
            let 
                attackConc = rmdups . concat $ subBodys language . neg . ruleHead <$> initAL
                negRuleAsHead = concat $ ruleAsHead language . neg <$> attackConc ++ initAL
                attack = rmdups . concat $ subBodys language <$> negRuleAsHead ++ attackConc ++ initAL
            in 
                if attack == endAL 
                    then attack 
                    else computeASG lag attack attack 