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

import Space.Language (Language,StrictRules(..),DefeasibleRules(..))
import Space.Argumentation (ArgumentationSpace, PreferenceSpace) 

data Env = Env 
    { langSpace :: Language 
    , sRuleSpace :: StrictRules
    , dRuleSpace :: DefeasibleRules
    , arguSpace :: ArgumentationSpace
    , prefSpace :: PreferenceSpace 
    } 

instance Show Env where 
    show env = 
        "LanguageSpace: " ++ show (langSpace env) ++ "\n" ++
        "Strict Rules: " ++ show (getStrictRules $ sRuleSpace env) ++ "\n" ++ 
        "Defeasible Rules: " ++ show (getDefeasibleRules $ dRuleSpace env) ++ "\n" ++ 
        "ArgumentationSpace: " ++ show (arguSpace env) ++ "\n" ++ 
        "Preferrence Space: " ++ show (prefSpace env) 

class Has field env where 
    obtain :: env -> field 

grab :: forall field env m . (MonadReader env m , Has field env) => m field 
grab = asks $ obtain @field 

instance Has Language Env where obtain = langSpace 
instance Has StrictRules Env where obtain = sRuleSpace
instance Has DefeasibleRules Env where obtain = dRuleSpace
instance Has ArgumentationSpace Env where obtain = arguSpace 
instance Has PreferenceSpace Env where obtain = prefSpace

type UseRuleOnly env = (Has StrictRules env, Has DefeasibleRules env)

newtype App a = App 
    { unApp :: ReaderT Env IO a 
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader Env) 

runApp :: Env -> App a -> IO a 
runApp env app = (runReaderT $ unApp app) env 