{-# LANGUAGE 
 TemplateHaskell
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      : None
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Just a test class for the main library.
module Main where

import Data.Functor
import Language.XmlHtml.HtmlTags
import Language.XmlHtml.XmlWriter
import Network
import Control.Concurrent
import System.IO
import System.Posix.Signals

handler sock = do
  sClose sock
  putStrLn "Ending Program"
  
main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 1338
  installHandler sigINT (Catch $ handler socket) Nothing
  sequence_ $ repeat $ do
    (h,_,_) <- accept socket
    forkIO $ do
      t <- page <$> msg
      hPutStr h $ t
      hFlush h
      hClose h

page content = "HTTP/1.0 200 OK\r\nContent-Length: "++show (length content)++"\r\n\r\n"++content++"\r\n"

msg = makeXml $                           
  html $ do
    
    liftIO $ putStrLn "hi - why is this printing so many times?"
    
    body $ name "thisbody" $ do
      h1 "AHA better syntax bitches!"
      p $ name "dig" $ "HEHE and paragraphs!"
      anchor $ name "dog" $ href "http://www.hulu.com/" $ "And links"
