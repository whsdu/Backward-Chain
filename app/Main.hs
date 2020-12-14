module Main where
import System.Environment
import System.Exit

import qualified Space.Language as L 
import Utility.PathSearcher 
import Parser.FileParser
import Env 
import Data.List (intercalate)

main = do 
    args <- getArgs 
    if length args /= 2
        then usage >> exit
        else search args


search [file,query] = do 
    env <- parseEnv file
    let 
        qm = parseLiteralMap env
        pfmap = parsePreferenceMap env
        a0 = parseQueryLiteral query qm
        rules = L.getDefeasibleRules ( envDRuleSpace env) ++ L.getStrictRules (envSRuleSpace env)
        run = runApp env
    efp <- run $ querySingleConclusion a0
    putStrLn . showGroup  $ evenLayer' rules pfmap efp

showGroup :: [L.EquifinalPaths] -> String
showGroup group = intercalate "\n" $ show <$> concat group 

usage :: IO ()
usage   = putStrLn "Usage: DT-exe fileName query"
exit :: IO a
exit    = exitSuccess