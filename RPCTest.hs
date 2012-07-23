{-# LANGUAGE 
 TemplateHaskell, 
 ScopedTypeVariables
 #-}
module Main where
import RPC

data Server = Server
instance Host Server where
  getLocation _ = "Server"
  getPort _ = 9000
  getValue = Server

data Client = Client
instance Show Client
instance Host Client where
  getLocation _ = "Client"
  getPort _ = 9000
  getValue = Client

data Person = IamAPerson String deriving (Read, Show)

putText str = liftIO $ putStrLn str

getText str = putText str >> liftIO getLine

databaseService nm password = do
  Server <- world
  pass <- getText $ "password for " ++ nm ++"?"
  return $ if pass == password
           then Just $ IamAPerson nm
           else Nothing

clientPage = do
  Client <- world
  nm <- getText "name?"
  pass <- getText "password?"
  person <- remoteCall databaseService nm pass
  putText $ show person

data LocalHost = LocalHost 
instance Host LocalHost where
  getLocation _ = "localhost"
  getPort _ = 8000
  getValue = LocalHost
  
justServer = do
  LocalHost <- world
  r <- newRef (1337 :: Integer)
  v <- fetchRefValue r
  
  incRef <- newRef ((+ 1) :: Integer -> Integer)
  putText $ "value: " ++ show v
  incr <- fetchRefValue incRef
  putText $ "incr value: " ++ show (incr v)
  
  addRef <- newRef ((+) :: Integer -> Integer -> Integer)
  add <- fetchRefValue addRef
  putText $ "add value: " ++ (show $ add v v)


main = runServer justServer
  