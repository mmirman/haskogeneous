{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module ModalHaskellTest where
import ModalHaskell

ax1 :: forall a b w . Host w => (Ref a -> Box b) -> Box (a -> WIO w b)
ax1 f = Box $ do
  w' <- world
  return $ \y -> do
    w <- world
    r <- fetchMobile w $ do
      a <- getRemoteRef w' $ newRef y
      return $ f a
    unbox r

ax2 :: Host host => Ref (Box a) -> WIO host (Box a)
ax2 r = letRemote r $ \w y -> fetchMobile w (return y)

ax3 :: Ref a -> Box (Ref a)
ax3 ref = Box $ return ref


{-

$(build [d|


         rpc' :: Box Int -> WIO Server Int
         rpc' a = do
           a' <- unbox a
           LiftIO $ putStrLn $ show $ a'
           return a'
         
         main' :: WIO Client Int
         main' = do
           a <- LiftIO readLn
           ar <- newRef a
           r <- fetchMobile Server $ do
             a'' <- fetchMobile Client $ do
               ak <- letRemote ar (const return)
               return $ Box $ return ak
             b <- rpc'  a''
             return $ Box $ return b
           r' <- unbox r
           LiftIO $ putStrLn $ show r'
           return a


       
          |])
-}