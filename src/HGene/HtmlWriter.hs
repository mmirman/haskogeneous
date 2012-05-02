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
                          
                          -- ** tags
                          
                        , h1  
                        , p
                        , body
                        , html
                        , link
                        , script
			, bold
                        ) where 

import Unsafe.Coerce
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Control.Monad.Writer (Writer, tell, execWriter)
import HGene.JSCompiler.HaskellToJavaScript


void a = a>> return ()                                                
-- --------------------------------------------------------------------------
-- Type definition for the HtmlWriter
type HtmlWriter = Writer String

-- | @'writeString' s@ writes a string to the html writer monad
writeString = tell

-- | @'makeHtml' s@ currently just converts this into a string
makeHtml = execWriter

-- --------------------------------------------------------------------------
-- Printable allows us to use tag for either a monad or a string or whatever
-- just makes syntax better, and ideally in the future, everything better.
class Printable a where printThis :: a -> HtmlWriter ()
instance Printable [Char] where printThis = writeString
instance Printable (HtmlWriter a) where printThis = void

newtype Html = Html { getHtmlElements :: [HtmlElement] }
data HtmlElement
      = HtmlString String
      | HtmlTag {                   -- tag with internal markup
              markupTag      :: String,
              markupAttrs    :: [HtmlAttr],
              markupContent  :: Html
              }
data HtmlAttr = HtmlAttr String String

param_tag tg param msg = do
  writeString $ "<"++tg++" "++param++">\n"
  printThis msg
  writeString $ "</"++tg++">\n"

tag tg = param_tag tg ""

-- HTML tags
address             =  tag "ADDRESS"
anchor              =  tag "A"
applet              =  tag "APPLET"
big                 =  tag "BIG"
blockquote          =  tag "BLOCKQUOTE"
body                =  tag "BODY"
bold                = tag "BOLD"
caption             =  tag "CAPTION"
center              =  tag "CENTER"
cite                =  tag "CITE"
ddef                =  tag "DD"
define              =  tag "DFN"
dlist               =  tag "DL"
dterm               =  tag "DT"
emphasize           =  tag "EM"
fieldset            =  tag "FIELDSET"
font                =  tag "FONT"
form                =  tag "FORM"
frame               =  tag "FRAME"
frameset            =  tag "FRAMESET"
h1                  =  tag "H1"
h2                  =  tag "H2"
h3                  =  tag "H3"
h4                  =  tag "H4"
h5                  =  tag "H5"
h6                  =  tag "H6"
header              =  tag "HEAD"
html                =  tag "HTML"
h_code              =  tag "CODE"
h_div               =  tag "DIV"
h_link              =  tag "LINK"
h_map               =  tag "MAP"
h_span              =  tag "SPAN"
h_title             =  tag "TITLE"
italics             =  tag "I"
keyboard            =  tag "KBD"
legend              =  tag "LEGEND"
li                  =  tag "LI"
noframes            =  tag "NOFRAMES"
olist               =  tag "OL"
option              =  tag "OPTION"
p                   =  tag "P"
pre                 =  tag "PRE"
sample              =  tag "SAMP"
select              =  tag "SELECT"
small               =  tag "SMALL"
strong              =  tag "STRONG"
style               =  tag "STYLE"
sub                 =  tag "SUB"
sup                 =  tag "SUP"
table               =  tag "TABLE"
td                  =  tag "TD"
textarea            =  tag "TEXTAREA"
th                  =  tag "TH"
tr                  =  tag "TR"
tt                  =  tag "TT"
ulist               =  tag "UL"
underline           =  tag "U"
variable            =  tag "VAR"

link lk = param_tag "a" ("href="++show lk)
script = appE [| tag "script" |] . hsToJs

