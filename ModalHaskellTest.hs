{-# LANGUAGE TemplateHaskell #-}
module ModalHaskellTest where
import ModalHaskell

-- hello world
$(build [d|
  
  rpc a = return a       
  
  main :: Client Int
  main = do
    a <- LiftIO readLn
    toServer a $ Box rpc
          |])


