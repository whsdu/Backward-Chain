{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}


module Env where 

import Control.Monad.Reader 

import Space.Language (Language,StrictRules(..),DefeasibleRules(..), PreferenceMap) 
import Space.Argumentation (ArgumentationSpace ) 

-- | TODOs: 
-- 1. do we really need to wrap rules as StrictRules and DefeasibleRules respectively. 
-- 2. do we really need the envArguSpace here ? 
-- 3. a new PrefSpace is needed, this should be a function that given to literal and return true if
-- the first is at least prefer as the snd arg.
data Env = Env 
    { envLangSpace :: Language 
    , envSRuleSpace :: StrictRules
    , envDRuleSpace :: DefeasibleRules
    , envArguSpace :: ArgumentationSpace
    , envPrefMap:: PreferenceMap
    } 

instance Show Env where 
    show env = 
        "LanguageSpace: " ++ show (envLangSpace env) ++ "\n" ++
        "Strict Rules: " ++ show (getStrictRules $ envSRuleSpace env) ++ "\n" ++ 
        "Defeasible Rules: " ++ show (getDefeasibleRules $ envDRuleSpace env) ++ "\n" ++ 
        "ArgumentationSpace: " ++ show (envArguSpace env) ++ "\n" ++ 
        "Preferrence Space: " ++ show (envPrefMap env) 

class Has field env where 
    obtain :: env -> field 

grab :: forall field env m . (MonadReader env m , Has field env) => m field 
grab = asks $ obtain @field 

instance Has Language Env where obtain = envLangSpace 
instance Has StrictRules Env where obtain = envSRuleSpace
instance Has DefeasibleRules Env where obtain = envDRuleSpace
instance Has ArgumentationSpace Env where obtain = envArguSpace 
instance Has PreferenceMap Env where obtain = envPrefMap

type UseRuleOnly env = (Has StrictRules env, Has DefeasibleRules env)

newtype App a = App 
    { unApp :: ReaderT Env IO a 
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader Env) 

runApp :: Env -> App a -> IO a 
runApp env app = (runReaderT $ unApp app) env 
