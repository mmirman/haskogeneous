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
import HGene.JSCompiler.JSBase 
import HGene.HtmlWriter
import Network
import Control.Concurrent
import System.IO
import System.Posix.Signals

handler sock = do
  sClose sock
  putStrLn "Ending Program"

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 1337
  installHandler sigINT (Catch $ handler socket) Nothing
  sequence_ $ repeat $ do
    (h,_,_) <- accept socket
    forkIO $ do
      hPutStr h $ page msg
      hFlush h
      hClose h

page content = "HTTP/1.0 200 OK\r\nContent-Length: "++show (length content)++"\r\n\r\n"++content++"\r\n"

msg = makeHtml $                                                                   
  html $ do
    body $ do                             
      h1 "AHA better syntax bitches!"                              
      p "HEHE and paragraphs!"
{-      p "<script>\n\
        \ var final = \n\
        \  function (){\n\
        \       var value = 0; \n\
        \       var used = 0; \n\
        \       return function (){ \n\
        \                  if (!used) { \n\
        \                      value = alert(5); \n\
        \                      used = 1;\n\
        \                  }\n\
        \                  return value; \n\
        \               };\n\
        \   }();\n\
        \ final();\n\
        \ final();\n\
        \</script>" -}
      script [| (\x -> x) (alert 5) |]
      
--      script [| (\(x,yp) y -> y (x * yp)) (4,5) alert |]
