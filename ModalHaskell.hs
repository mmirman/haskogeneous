{-# LANGUAGE 
 FlexibleInstances,
 UndecidableInstances,
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 ViewPatterns,
 StandaloneDeriving,
 GADTs
 #-}
module ModalHaskell ( Box(..)
                    , build
                    , Data(..)
                    , Client(..)
                    ) where

import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M

newtype WIO world a = LiftIO (IO a)
deriving instance Monad (WIO world)
deriving instance Functor (WIO world)
deriving instance MonadIO (WIO world)

newtype Box a = Box { unbox :: a } -- mobile
data Ref a where 
  Here :: Host host => host ->  (IORef a) -> Ref a -- Diagonal 

-- | Data is anything that is serializable - that you can both show and read
class Data a
instance (Show a, Read a) => Data a


data Client = Client deriving Read
instance Show Client where show _ = "Client"


class Host a where
  getLocation :: a -> String
  getValue :: a

instance (Show a, Read a) => Host a where
  getLocation _ = show (undefined :: a)
  getValue = read $ show (undefined :: a)

-- newRef =~= "here"
newRef :: forall a host . Host host => a -> WIO host (Ref a)
newRef a = do
  r <- liftIO $ newIORef a
  return $ Here (getValue :: host) r

-- letRemote =~= "letd"
-- not actually how this should work.  a remote request should be made.
letRemote :: Host host' => Ref a -> (a -> WIO host' b) -> WIO host' b
letRemote (Here host ref) = (>>=) $ liftIO $ readIORef ref

-- getRemoteRef =~= "get"
-- not actually how this should work.  a remote request should be made
getRemoteRef :: (Host hostA, Host hostB) => WIO hostA (Ref a) -> WIO hostB (Ref a)
getRemoteRef (LiftIO m) = (LiftIO m)

-- fetchMobile =~= "fetch"  
fetchMobile :: (Host hostA, Host hostB) => WIO hostA (Box a) -> WIO hostB (Box a)
fetchMobile (LiftIO m) = (LiftIO m)  




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
