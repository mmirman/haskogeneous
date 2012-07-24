{-# LANGUAGE
 TemplateHaskell,
 MultiParamTypeClasses
 #-} 
module MobileTransform ( build
                       , makeHost
                       , HostDerive
                       ) where

import Language.Haskell.TH

class HostDerive a b c

makeHost :: String -> String -> Integer -> Q [Dec]
makeHost n l p = do
  let nm = mkName n
  let host = mkName "Host"
  dat <- dataD (cxt []) nm [] [normalC nm []] []
  inst <- instanceD (cxt []) (appT (conT host) (conT nm))
          [ funD (mkName "getLocation") [clause [wildP] (normalB $ litE $ StringL l) []]
          , funD (mkName "getPort") [clause [wildP] (normalB $ litE $ IntegerL p) []]
          , funD (mkName "getValue") [clause [] (normalB $ conE nm) []]
          ]
  return [ dat
         , inst
         ]

build :: Q [Dec] -> Q [Dec]
build m = do
  dlist <- m
  dl <- mapM act dlist
  return $ concat dl
  
act :: Dec -> Q [Dec]

act v = return [v]