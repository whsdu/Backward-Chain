module BC where 

import Language 
import Argumentation 
import LAParser 


funcAL :: LanguageSpace -> [String] -> Literal -> ArgumentationSpace
funcAL languageSpace argNames l = 
    let 
        prod = subBodys languageSpace l 
    in parsBasicArgument prod argNames 

funcASG :: LanguageSpace -> [Literal] -> (ArgumentationSpace, PreferrenceSpace)
funcASG = undefined 