{-# LANGUAGE 
 FlexibleInstances,
 UndecidableInstances,
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables
 #-}
module ModalHaskell (Box(..), build, toServer, Data(..), Client(..)) where

import Language.Haskell.TH
import Data.Monoid
import qualified Data.Map as M

data Box a = Box { unbox :: a }

class Data a

instance (Show a, Read a) => Data a

newtype Client host a = LiftIO (IO a)
                      deriving (Monad)

class Host a where  
  getLocation :: a -> String


instance Host String where
  
  
-- A temporary definition of the monad operations.  This should change significantly.
toServer :: forall a b host . (Host host, Data a, Data b) => a -> Box (a -> IO b) -> Client host b
toServer a (Box f) = LiftIO (f a)
  where hostName = getLocation (undefined :: host)


build :: Q [Dec] -> Q [Dec]
build ml = do
  d <- ml
  mapM (check 0 mempty) d

type BoxLevel = Integer

type LocalContext = M.Map Name BoxLevel

class Check a where
  check :: BoxLevel -> LocalContext -> a -> Q a

instance Check Dec where
  check boxlevel ctxt a = case a of
    FunD nm _ -> undefined
    ValD pat bd decs -> undefined
    a -> return a

instance Check Name where
  check boxlevel ctxt nm = case M.lookup nm ctxt of
    Just i | i <= boxlevel -> do
      loc <- location
      error $ "nm: " ++ show nm 
        ++ " @ " ++ show (loc_start loc)
        ++ " in "++ loc_filename loc
        ++ "\n Don't you wish this had better error reporting?" 
        ++ "\nWhy doesn't template haskell include statistics with the expressions?"
    _ -> return nm
