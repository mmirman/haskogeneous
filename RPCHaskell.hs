{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 StandaloneDeriving,
 RankNTypes,
 UndecidableInstances,
 TypeFamilies, 
 MultiParamTypeClasses
 #-}
module RPCHaskell ( WIO(..)
                  , world
                  , remoteCall
                  , Host()
                  , Sendable()
                  , Ref()
                  , newRef
                  , fetchRefValue
                  ) where

import Language.Haskell.TH
import Data.Monoid
import Data.Functor 
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Concurrent
import System.Random
import Network
import Unsafe.Coerce
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

class (Read a, Show a) => Host a where
  getLocation :: a -> String
  getLocation _ = show (undefined :: a)
  getValue :: a
  getValue = read $ show (undefined :: a)

  
newtype WIO w a = LiftIO { unliftWIO :: IO a }
deriving instance Monad (WIO w)
deriving instance Functor (WIO w)
deriving instance MonadIO (WIO w)


world :: forall w . Host w => WIO w w
world = return (getValue :: w)

data Ref a = Ref String Integer deriving (Show, Read)

class Sendable a where
  getRefValue :: String -> Ref a -> IO a
  makeRefFrom :: String -> a -> IO (Ref a)
    
instance (Read a, Show a) => Sendable a where
  makeRefFrom w v = do
    let v' = show v
    ptr <- randomIO 
    liftIO $ forkIO $ do
      s <- listenOn $ PortNumber $ fromInteger ptr
      forever $ forkIO $ do
        (handle, host, port) <- accept s
        "" <- recvFrom host $ PortNumber port
        sendTo host (PortNumber port) v'
    return $ Ref w ptr
  {-# NOINLINE getRefValue #-}
  getRefValue _ (Ref w p) = do
    let p' = PortNumber $ fromInteger p
    sendTo w p' ""
    read <$> recvFrom w p'

instance (Sendable a, Sendable b) => Sendable (a -> b) where
  makeRefFrom w f = do
    ptr <- randomIO
    liftIO $ forkIO $ do
      let ptr' =  PortNumber $ fromInteger ptr
      s <- listenOn ptr'
      forever $ forkIO $ do
        (handle, host, port) <- accept s
        aRef  <- read <$> recvFrom host (PortNumber port)
        bVal <- f <$> getRefValue w aRef
        bRef <- makeRefFrom w bVal
        sendTo host (PortNumber port) (show bRef)
    return $ Ref w ptr

  {-# NOINLINE getRefValue #-}
  getRefValue w (Ref w' p) = do
    let p' = PortNumber $ fromInteger p
    return $ \a -> unsafePerformIO $ do
      aRef <- makeRefFrom w a
      sendTo w' p' (show aRef)
      bRef <- read <$> recvFrom w' p'
      getRefValue w bRef


fetchRefValue :: (Sendable a , Host w) => Ref a -> WIO w a
fetchRefValue ref = do
  w <- getLocation <$> world
  liftIO $ getRefValue w ref 
  
newRef :: (Sendable a , Host w) => a -> WIO w (Ref a)
newRef a = do
  w <- getLocation <$> world
  liftIO $ makeRefFrom w a


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
