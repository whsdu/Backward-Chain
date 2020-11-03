{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module GRI.GRI
    ( GRI (..)
    , Rs (..)
    , Ds (..)
    , gri
    , griSupportPath
    , griActivated
    , griNecessaryLanguage
    )
    where 

import Control.Monad.Reader

import Env 
import qualified Space.Meta as M 
import qualified Space.Language as L 
import qualified Utility.Language as LU


newtype Rs = Rs { unpackRs :: [(L.Language, L.Literal)] }
newtype Ds = Ds { unpackDs :: [(L.Literal, L.Literal)] }
data GRI = GRI 
    { griGetN ::L.Language
    , griGetRs :: Rs
    , griGetDs :: Ds
    }

instance Show GRI where 
    show gri = 
                "N in GRI: " ++  show (griGetN gri) ++ "\n" ++ 
                "Rs in GRI: " ++  show (unpackRs . griGetRs $ gri) ++ "\n" ++
                "Ds in GRI: " ++  show (unpackDs . griGetDs $ gri) 
gri :: 
    ( MonadReader env m 
    , UseRuleOnly env 
    , MonadIO m
    ) => m GRI 
gri = do 
    sRule <- L.getStrictRules <$> grab @L.StrictRules
    dRule <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    let 
        rules = sRule ++ dRule 
        rs = Rs $ getRs rules <$> rules 
        ds = Ds $ getDs rules 
    pure $ GRI rules rs ds 
    where 
        getRs :: L.Language -> L.Literal -> (L.Language, L.Literal)
        getRs lang n = 
            let 
                supportN = [l | l <- lang, L.conC l `elem` L.body n]
            in (supportN, n)
        getDs :: L.Language -> [(L.Literal, L.Literal)]
        getDs lang = do 
            a <- lang 
            b <- lang 
            guard $ a `LU.isAttack` b 
            pure (a,b)

-- | It seems only Atom has no support path.
-- This is based on LU.langAL and basically the same.
-- TODOs: 
-- 1. Test if multiple rule draw the same conclusion, like in the Tree structure     
-- 2. In the `if` expression below: it would be quite impossible that `n notElem of snd rs`
griSupportPath ::
    ( LU.LanguageContext m 
    , MonadIO m 
    ) => GRI -> L.Literal -> m [L.Language]
griSupportPath gri n = 
    let 
        c = L.conC n 
        rs = unpackRs . griGetRs $ gri 
    in if n `notElem` (snd <$> rs) 
            then pure []
        else 
            do  
                supportlang <- LU.langAL c 
                pure . M.rmdups $ [ fst r  | r <- rs, snd r `elem` supportlang] ++ [[n]]

griNecessaryLanguage 
    :: 
    ( LU.LanguageContext m 
    , MonadIO m
    ) => GRI -> L.Literal -> m L.Language
griNecessaryLanguage gri l = do 
    supportLang <- LU.langAL l 
    let 
        rd = unpackDs . griGetDs $ gri 
        defender = [ snd r | r <- rd, fst r `elem` supportLang]
        attacker = [ fst r | r <- rd, snd r `elem` supportLang]
    pure $ M.rmdups $ supportLang ++ defender ++ attacker 

-- | Properties of GRI
--  Redundent in constructing BC
griActivated :: 
    ( LU.LanguageContext m 
    , MonadIO m
    ) => GRI -> L.Literal -> m Bool 
griActivated gri n = do 
    supportPath <- griSupportPath gri n 
    pure $ [n] `elem` supportPath 

-- | property of GRI 
-- Redundent in constructing BC
-- griConnected :: 

