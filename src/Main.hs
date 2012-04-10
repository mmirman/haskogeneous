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

import HGene.HtmlWriter
import Network
import Control.Concurrent
import System.IO

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 1337
  loop sock

loop sock = do
  (h,_,_) <- accept sock
  forkIO $ body h
  loop sock
  where
    body h = do
      hPutStr h $ page msg
      hFlush h
      hClose h

page content = "HTTP/1.0 200 OK\r\nContent-Length: "++show (length content)++"\r\n\r\n"++content++"\r\n"

msg = makeHtml $                                                                   
  html $ do                                                                        
    body $ do                                                                      
      h1 "AHA better syntax bitches!"                                              
      p "HEHE and paragraphs!"