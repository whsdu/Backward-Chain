module FileParser where 

import System.IO 
import Data.List.Split (splitOn)
import  qualified Data.HashMap.Strict  as Map 
import Language (Literal (..), LanguageSpace)
import MetaDefinition 

data Knowledge = Knowledge 
    { ruleName :: String
    , primisesName :: [String]
    , impName :: String
    , conclusionName :: String
    , preferName :: String 
    }deriving Show 

type KnowledgeSpace = [Knowledge]

testPath :: String 
testPath = "./Examples/Teams/"
testFile :: String 
testFile = "b2.txt"

parseWord :: String -> Knowledge
parseWord w = 
    let 
        [ruleName,ruleBody] = splitOn ":" w 
        impName 
            | '-' `elem` ruleBody = "->"
            | otherwise = "=>"
        [premies,conC] = splitOn impName ruleBody 
        premisesName = splitOn "," premies 
        [conclusionName,preferName] = splitOn "," conC
    in Knowledge ruleName premisesName impName conclusionName preferName

k2l :: KnowledgeSpace -> LanguageSpace 
k2l knowledges = constructLS knowledges Map.empty
    where 
        constructLS (k:ks) lsAcc = 
            let 
                concName = conclusionName k 
                priNames = primisesName k 
                rName = ruleName k 
                iName = impName k 
                (updateAtomAcc,primLiterals,concLiteral) = insertAtomsToLanguageSpace concName  priNames lsAcc
                updateRuleAcc = insertRuleToLanguageSpace rName iName primLiterals concLiteral updateAtomAcc
            in constructLS ks updateRuleAcc
        constructLS [] lsAcc  = lsAcc

insertAtomsToLanguageSpace :: String -> [String] -> LanguageSpace -> (LanguageSpace, [Literal], Literal)
insertAtomsToLanguageSpace concName priNames ls = 
    let 
        (accPrim, primLiterals) = foldr insertOneAtom (ls,[]) priNames 
        (accConc, concLiterals) = insertOneAtom concName (accPrim,[])
    in (accConc, primLiterals, head concLiterals)
    where insertOneAtom n (ll,lbs) = 
                    case Map.lookup n ll of 
                        Just b -> (ll, b:lbs)
                        Nothing -> 
                            let newl = Atom n 
                            in (Map.insert n newl ll, newl:lbs)

-- | TODOs: 
-- 1. handle with preference 
-- 2. read papers see the details of algorithm 
-- 3. summarize the conclusion 
insertRuleToLanguageSpace 
    :: String
    -> String 
    -> [Literal]
    -> Literal  
    -> LanguageSpace 
    -> LanguageSpace
insertRuleToLanguageSpace ruleName imp primies conclusion lspace =
    let 
        ruleBody = Map.lookup ruleName lspace 
        impSym = if imp == "->" then S else D 
    in  case ruleBody of 
        Just b -> case b of 
                    Atom _ -> undefined 
                    Rule {} -> undefined 
        Nothing -> 
            let 
                ruleLiteral = Rule ruleName primies impSym conclusion 
            in Map.insert ruleName ruleLiteral lspace 



readTestFile :: IO [Knowledge]
readTestFile = do
    handle <- openFile (testPath ++ testFile) ReadMode
    contents <- hGetContents handle 
    pure $ parseWord <$> words contents 