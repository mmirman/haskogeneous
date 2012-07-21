{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 ViewPatterns,
 StandaloneDeriving,
 GADTs,
 RankNTypes,
 UndecidableInstances
 #-}
module ModalHaskell ( Box(Box)
                    , unbox
                    , WIO(..)
                    , build
                    , world
                    , Client(..)
                    , Server(..)
                    , Ref()
                    , Host(..)
                    , newRef
                    , letRemote
                    , getRemoteRef
                    , fetchMobile
                    , Storable()
                    ) where

import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.Error
import Control.Monad.Reader
import Foreign hiding (unsafePerformIO)
import Foreign.Marshal.Utils
import Control.Concurrent
import System.Random
import Network
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

-------------------------------------------------------------------------------------------------
-- The library of functions
-------------------------------------------------------------------------------------------------

newtype WIO world a = LiftIO { unliftWIO :: (IO a) }
deriving instance Monad (WIO world)
deriving instance Functor (WIO world)
deriving instance MonadIO (WIO world)

newtype Box a = Box { unbox :: forall w . Host w => WIO w a } -- mobile

world :: forall w . Host w => WIO w w
world = return (getValue :: w)

data Ref a where
  -- this needs to be a StablePtr, although in the future it could be made
  -- more efficient by using castStablePtrToPtr
  Here :: Host host => host -> IntPtr {- =~= Ptr (StablePtr a) -} -> Ref a -- Diagonal 

class (Read a, Show a) => Host a where
  getLocation :: a -> String
  getLocation _ = show (undefined :: a)
  getValue :: a
  getValue = read $ show (undefined :: a)
  
data Client = Client deriving Read
instance Show Client where show _ = "Client"
instance Host Client

data Server = Server deriving (Read)
instance Show Server where show _ = "Server"
instance Host Server

-- newRef =~= "here"
newRef :: forall a host . (Host host) => a -> WIO host (Ref a)
newRef a = do
  r <- liftIO $ ptrToIntPtr <$> (new =<< newStablePtr a)
  return $ Here (getValue :: host) r

-- letRemote =~= "letd"
-- not actually how this should work.  a remote request should be made.
letRemote :: forall a b host'. Host host' => Ref a 
             -> (forall host . Host host => host -> a -> WIO host' b) -> WIO host' b
letRemote (Here host ref) f = f host =<< (liftIO $ deRefStablePtr =<< peek (intPtrToPtr ref))

-- getRemoteRef =~= "get"
-- not actually how this should work.  a remote request should be made
getRemoteRef :: (Host hostA, Host hostB) => hostA -> WIO hostA (Ref a) -> WIO hostB (Ref a)
getRemoteRef w (LiftIO m) = LiftIO m
  -- send m to w, compute m, return the value

-- fetchMobile =~= "fetch"
fetchMobile :: (Host hostA, Host hostB) => hostA -> WIO hostA (Box a) -> WIO hostB (Box a)
fetchMobile _ (LiftIO m) = (LiftIO m)

-------------------------------------------------------------------------------------------------
-- The pipeline
-------------------------------------------------------------------------------------------------
build :: Q [Dec] -> Q [Dec]
build ml = do
  decs <- ml
  r <- localCheck decs
  case r of 
    Left err -> error err
    Right () -> compile decs

-------------------------------------------------------------------------------------------------
-- The modal checker
-------------------------------------------------------------------------------------------------
type World = String
type LocalContext = M.Map Name World
type Context = (World, LocalContext)

type QCheck = ReaderT Context (ErrorT String Q)

getWorld :: QCheck World
getWorld = fst <$> ask

getCtxt :: QCheck LocalContext
getCtxt = snd <$> ask

success = return ()

localCheck :: [Dec] -> Q (Either String ())
localCheck decs = runErrorT (mapM_ checkDec decs)

checkDec dec = do
  runReaderT (check dec) ("",mempty)

liftQ = lift . lift
throw = lift . throwError

withWorld :: World -> QCheck a -> QCheck a
withWorld w = withReaderT (\(_,c) -> (w,c))

addVar :: Name -> World -> QCheck a -> QCheck a
addVar n w = withReaderT (\(w,c) -> (w,M.insert n w c))


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
    world <- getWorld
    case M.lookup nm ctxt of
      Just i | i /= world -> do
        loc <- liftQ location
        throw $ "nm: " ++ show nm 
          ++ " @ " ++ show (loc_start loc)
          ++ " in "++ loc_filename loc
          ++ "\n Don't you wish this had better error reporting?" 
          ++ "\nWhy doesn't template haskell include statistics with the expressions?"
      _ -> success

-------------------------------------------------------------------------------------------------
-- The compiler
-------------------------------------------------------------------------------------------------
compile :: [Dec] -> Q [Dec]
compile = return