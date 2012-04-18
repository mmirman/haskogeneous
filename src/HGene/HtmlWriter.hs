{-# LANGUAGE 
 FlexibleInstances, 
 TypeSynonymInstances, 
 NoMonomorphismRestriction,
 TemplateHaskell
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HGene.HtmlWriter
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Some tools for writing html in a pretty format in haskell.

module HGene.HtmlWriter ( HtmlWriter
                        , Printable (..)
                          
                          -- ** basic html writer interaction
                          
                        , writeString
                        , makeHtml
                          
                          -- ** creating new html tags
                          
                        , param_tag
                        , tag
                          
                          -- ** some example tags
                          
                        , h1  
                        , p
                        , body
                        , html
                        , link
                        , script
                        ) where 

import Unsafe.Coerce
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Control.Monad (void)
import Control.Monad.Writer (Writer, tell, execWriter)
import HGene.JSCompiler.HaskellToJavaScript


                                                
-- --------------------------------------------------------------------------
-- Type definition for the HtmlWriter
type HtmlWriter = Writer String

-- | 'writeString s' writes a string to the html writer monad
writeString = tell

-- | 'makeHtml s' currently just converts this into a string
makeHtml = execWriter

-- --------------------------------------------------------------------------
-- Printable allows us to use tag for either a monad or a string or whatever
-- just makes syntax better, and ideally in the future, everything better.
class Printable a where printThis :: a -> HtmlWriter ()
instance Printable [Char] where printThis = writeString
instance Printable (HtmlWriter a) where printThis = void

param_tag tg param msg = do
  writeString $ "<"++tg++" "++param++">\n"
  printThis msg
  writeString $ "</"++tg++">\n"

tag tg = param_tag tg ""

h1 = tag "h1"
p = tag "p"
body = tag "body"
html = tag "html"
link lk = param_tag "a" ("href="++show lk)

script = appE [| tag "script" |] . hsToJs