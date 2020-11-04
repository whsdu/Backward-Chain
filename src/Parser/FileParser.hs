module Parser.FileParser where 

import Control.Monad(guard)
import System.IO 
import Data.List.Split (splitOn)
import  qualified Data.HashMap.Strict  as Map 
import Data.Maybe(fromJust, fromMaybe)

import qualified Space.Language as L (Literal (..), Language , LanguageMap,StrictRules(..), DefeasibleRules(..),Preference(..), PreferenceSpace, name,body,imp,conC)
import qualified Space.Meta as M
import qualified Utility.Language as LU
import Env 


data Knowledge = Knowledge 
    { ruleName :: String
    , premisesName :: [String]
    , impName :: String
    , conclusionName :: String
    , preferName :: String 
    }deriving Show 

type KnowledgeSpace = [Knowledge]


parseEnv :: FilePath -> IO Env
parseEnv filePath = do 
    k <- stringToKnowledge filePath
    let 
        l = k2l k 
        r = chainingRule l 
    pure $ mkEnv r 

parseLiteralMap :: Env -> Map.HashMap M.Name L.Literal
parseLiteralMap env = 
    let 
        language = envLangSpace env 
    in Map.fromList $ zip (L.name <$> language ) language

parseQueryLiteral :: String -> Map.HashMap M.Name L.Literal -> L.Literal
parseQueryLiteral qName lm = fromJust $ Map.lookup qName lm 

----------------------
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

stringToKnowledge :: FilePath -> IO KnowledgeSpace
stringToKnowledge filePath = do
    handle <- openFile filePath  ReadMode
    contents <- hGetContents handle 
    pure $ parseWord <$> words contents 

k2l :: KnowledgeSpace -> L.LanguageMap 
k2l knowledges = constructLS knowledges Map.empty
    where 
        constructLS (k:ks) lsAcc = 
            let 
                concName = conclusionName k 
                priNames = premisesName k 
                rName = ruleName k 
                iName = impName k 
                (updateAtomAcc,primLiterals,concLiteral) = insertAtomsToLanguageSpace concName  priNames lsAcc
                updateRuleAcc = insertRuleToLanguageSpace rName iName primLiterals concLiteral updateAtomAcc
            in constructLS ks updateRuleAcc
        constructLS [] lsAcc  = lsAcc
        insertAtomsToLanguageSpace :: String -> [String] -> L.LanguageMap -> (L.LanguageMap, L.Language, L.Literal)
        insertAtomsToLanguageSpace concName priNames ls = 
            let 
                (accPrim, primLiterals) = foldr insertOneAtom (ls,[]) priNames 
                (accConc, concLiterals) = insertOneAtom concName (accPrim,[])
            in (accConc, primLiterals, head concLiterals)
            where insertOneAtom n (ll,lbs) = 
                            case Map.lookup n ll of 
                                Just b -> (ll, b:lbs)
                                Nothing -> 
                                    let newl = L.Atom n 
                                    in (Map.insert n newl ll, newl:lbs)
        insertRuleToLanguageSpace 
            :: String
            -> String 
            -> L.Language
            -> L.Literal  
            -> L.LanguageMap 
            -> L.LanguageMap
        insertRuleToLanguageSpace ruleName imp primies conclusion lspace =
            let 
                impSym = if imp == "->" then M.S else M.D 
                ruleLiteral = L.Rule ruleName primies impSym conclusion 
            in Map.insert ruleName ruleLiteral lspace 

chainingRule :: L.LanguageMap -> L.LanguageMap 
chainingRule knowledgeMap = 
    let 
        ruleList = [ km | km <- Map.toList knowledgeMap , (L.imp . snd) km == M.D || (L.imp . snd) km == M.S] 
        ruleMap = Map.fromList [ km | km <- Map.toList knowledgeMap , (L.imp . snd) km == M.D || (L.imp . snd) km == M.S] 
    in Map.fromList $ chaining ruleMap <$> ruleList
    where 
        chaining rm tp = 
            let
                key = fst tp 
                p = snd tp 
                name = L.name p 
                imp = L.imp p 
                body = L.body p 
                conC = L.conC p 
                newBody = searchRules rm <$> body 
                newConc = searchRules rm conC 
            in (key, L.Rule name newBody imp newConc)
        searchRules :: L.LanguageMap -> L.Literal -> L.Literal 
        searchRules rm l = 
            case l of 
                L.Rule {} -> l 
                L.Atom "" -> l 
                L.Atom _ -> 
                    let 
                        name = L.name l 
                    in 
                        if head name == '!' 
                            then 
                                let 
                                    oName = tail name 
                                    rO = Map.lookup oName rm 
                                in case rO of 
                                    Nothing -> l 
                                    Just rule -> M.neg rule 
                            else 
                                let r = Map.lookup name rm 
                                in fromMaybe l r 

mkEnv :: L.LanguageMap -> Env 
mkEnv lm = 
    let 
        literalList = snd <$> Map.toList lm 
        strictRule = L.StrictRules [ l | l <- literalList, L.imp l == M.S]
        defeasibleRule = L.DefeasibleRules [ l | l <- literalList, L.imp l == M.D]
        atoms = M.rmdups [ l | l <- concat (L.body <$> literalList) ++ (L.conC <$> literalList), L.imp l == M.N]
        demoPrefer = makeDemoPrefer (L.getStrictRules strictRule ++ L.getDefeasibleRules defeasibleRule)
    in Env (atoms ++ L.getStrictRules strictRule ++ L.getDefeasibleRules defeasibleRule) strictRule defeasibleRule [] demoPrefer
    where 
        makeDemoPrefer :: L.Language -> L.PreferenceSpace
        makeDemoPrefer lang = do 
                        a <- lang 
                        b <- lang 
                        guard $ a `LU.isRebutting`b 
                        guard $ head (L.name (L.conC a)) /= '!'
                        pure $ L.Preference a b 