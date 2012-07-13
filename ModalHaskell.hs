{-# LANGUAGE 
 FlexibleInstances,
 UndecidableInstances,
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 ViewPatterns,
 DataKinds,
 KindSignatures
 #-}
module ModalHaskell (Box(..), build, toServer, Data(..), Client(..)) where

import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.Reader
import qualified Data.Map as M

newtype Box a = Box { unbox :: a }
data Dia host a = Here host a

here :: Host host => a -> Dia host a
here a = Here getValue a

class Data a

instance (Show a, Read a) => Data a

newtype Client a = LiftIO (IO a)
                 deriving (Monad)

class Host a where  
  getLocation :: a -> String
  getValue :: a

data Loc = Loc deriving (Show, Read)

instance (Show a, Read a) => Host a where
  getLocation _ = show (undefined :: a)
  getValue = read $ show (undefined :: a)

-- A temporary definition of the monad operations.  This should change significantly.
toServer :: forall a b host . (Host host, Data a, Data b) => a -> Box (Dia host (a -> IO b)) -> Client b
toServer a (Box (Here host f)) = LiftIO (f a)
  where hostName = getLocation (undefined :: host)

build :: Q [Dec] -> Q [Dec]
build ml = do
  decs <- ml
  localCheck decs
  return decs
  
type BoxLevel = Integer
type LocalContext = M.Map Name BoxLevel
type Context = (BoxLevel, LocalContext)
type QCheck = ReaderT Context Q

getBoxLevel :: QCheck BoxLevel
getBoxLevel = fst <$> ask

getCtxt :: QCheck LocalContext
getCtxt = snd <$> ask

success = return ()

localCheck :: [Dec] -> Q ()
localCheck decs = runReaderT (mapM_ check decs) (0,mempty)

class Check a where
  check :: a -> QCheck ()

instance Check Dec where
  check a = case a of
    FunD nm _ -> undefined
    ValD pat bd decs -> undefined
    a -> success

instance Check Exp where
  check a =
    case a of
      VarE nm -> check nm
      AppE (ConE (nameBase -> "Box")) _ -> undefined -- Watch out below!
      AppE a b -> do
        check a
        check b        
      ConE nm -> success
      ParensE e -> check e

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
      _ -> success
