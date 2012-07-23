{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 StandaloneDeriving,
 UndecidableInstances,
 TypeFamilies, 
 MultiParamTypeClasses,
 IncoherentInstances
 #-}
module RPC ( WIO()
           , world
           , remoteCall
           , Host(..)
           , Sendable()
           , Ref()
           , newRef
           , fetchRefValue
           , liftIO
           , runServer
           ) where

import MultiServer
import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Concurrent
import System.Random
import Network
import Unsafe.Coerce
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

class Host a where
  getLocation :: a -> String
  getPort :: a -> Integer
  getValue :: a
  
newtype WIO w a = LiftAIO { unliftWIO :: AIO a }
deriving instance Monad (WIO w)
deriving instance Functor (WIO w)
deriving instance MonadIO (WIO w)

runServer :: forall w . Host w => WIO w () -> IO ()
runServer = startServer (getPort (undefined :: w)) . unliftWIO 

world :: forall w . Host w => WIO w w
world = return (getValue :: w)

data Ref a = Ref String Integer ServiceID
           | Val String
           deriving (Show, Read)

class Sendable a where
  getRefValue :: Host w => w -> Ref a -> AIO a
  makeRefFrom :: Host w => w -> a -> AIO (Ref a)
    
instance (Read a, Show a) => Sendable a where
  makeRefFrom _ v = return $ Val (show v)
  getRefValue _ (Val s) = return $ read s

instance (Sendable a, Sendable b) => Sendable (a -> b) where
  makeRefFrom w f = do
    ptr <- addService $ \handle -> do
          aRef <- recv handle
          bVal <- f <$> getRefValue w aRef
          bRef <- makeRefFrom w bVal
          send handle bRef
    return $ Ref (getLocation w) (getPort w) ptr

  {-# NOINLINE getRefValue #-}
  getRefValue w (Ref w' p s) = do

    state <- getHandlers 
    return $ \a -> unsafePerformServer state $ do
      aRef <- makeRefFrom w a
      handle <- connectToService w' p s
      send handle aRef
      bRef <- recv handle
      getRefValue w bRef

fetchRefValue :: (Sendable a , Host w) => Ref a -> WIO w a
fetchRefValue ref = do
  w <- world
  LiftAIO $ getRefValue w ref 
  
newRef :: (Sendable a , Host w) => a -> WIO w (Ref a)
newRef a = do
  w <- world
  LiftAIO $ makeRefFrom w a


class Host w => RPC a w where
  type Initialize a w
  
  remoteCall :: a -> Initialize a w
  remoteCall = undefined
  
  realRemoteCall :: a -> w -> String -> Initialize a w

  doFirst :: a -> w -> String -> WIO w () -> Initialize a w -> Initialize a w
  

instance (Sendable a, Host w, Host w') => RPC (WIO w a) w' where
  
  type Initialize (WIO w a) w' = WIO w' a
  
  realRemoteCall _ _ nm = do
    let host = getLocation (getValue :: w)
    undefined
    
  doFirst _ _ nm ma f = ma >> f

  
instance (Sendable a, RPC b w') => RPC (a -> b) w' where
  type Initialize (a -> b) w' = a -> Initialize b w'
  
  realRemoteCall f1 w nm a = doFirst (undefined :: b) w nm putVal (realRemoteCall (undefined :: b) w nm)
    where putVal = do
            r <- newRef a
            undefined
            
  doFirst _ w nm ma f a = doFirst (undefined :: b) w nm ma (f a)
  
-- Now we can assure that remoteCall acts only on names with a build thing.  
-- remoteCall can then be altered to give the name of the function along with it's address to 

