{-# LANGUAGE 
 FlexibleInstances,
 UndecidableInstances,
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 ViewPatterns
 #-}
module ModalHaskell (Box(..), build, toServer, Data(..), Client(..)) where

import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.Reader
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
  runReaderT (mapM check d) (0,mempty)

type BoxLevel = Integer
type LocalContext = M.Map Name BoxLevel
type Context = (BoxLevel, LocalContext)
type QCheck = ReaderT Context Q

getBoxLevel :: QCheck BoxLevel
getBoxLevel = fst <$> ask

getCtxt :: QCheck LocalContext
getCtxt = snd <$> ask

class Check a where
  check :: a -> QCheck a

instance Check Dec where
  check a = case a of
    FunD nm _ -> undefined
    ValD pat bd decs -> undefined
    a -> return a

instance Check Exp where
  check a =
    case a of
      VarE nm -> VarE <$> check nm
      AppE (ConE (nameBase -> "Box")) _ -> undefined -- Watch out below!
      AppE a b -> do
        a' <- check a
        b' <- check b
        return $ AppE a' b'
      ConE nm -> return $ ConE nm
      ParensE e -> ParensE <$> check e

instance Check Name where
  check nm = do
    ctxt <- getCtxt
    boxlevel <- getBoxLevel
    case M.lookup nm ctxt of
      Just i | i <= boxlevel -> do
        loc <- lift location
        error $ "nm: " ++ show nm 
          ++ " @ " ++ show (loc_start loc)
          ++ " in "++ loc_filename loc
          ++ "\n Don't you wish this had better error reporting?" 
          ++ "\nWhy doesn't template haskell include statistics with the expressions?"
      _ -> return nm
