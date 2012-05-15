{-# LANGUAGE 
 FlexibleInstances, 
 TypeSynonymInstances, 
 NoMonomorphismRestriction,
 TemplateHaskell,
 GeneralizedNewtypeDeriving,
 FlexibleContexts
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HGene.HtmlWriterInternals
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Some tools for writing html in a pretty format in haskell.

-- TODO: Use the module redirect tequnique to exclude items we don't want.
module HGene.HtmlWriterInternals where
       
import Language.Haskell.TH
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.Writer.Class (MonadWriter(..))
import HGene.JSCompiler.HaskellToJavaScript


void a = a >> return ()

-- --------------------------------------------------------------------------
-- Type definition for the HtmlWriter
newtype HtmlWriter a = HtmlWriter {getHtml :: (Writer String a) }
                     deriving (Monad, MonadWriter String)
                              
--instance MonadWriter String (HtmlWriter a) where
  

-- | @'writeString' s@ writes a string to the html writer monad
writeString = tell

-- | @'makeHtml' s@ currently just converts this into a string
makeHtml = execWriter . getHtml . printThis



data (Param a) = Param [HtmlAttr] a

data Elem a = Elem String a

data HtmlAttr = Attr String String

instance Show HtmlAttr where
  show (Attr a b) = a++"="++b

-- --------------------------------------------------------------------------
-- Printable allows us to use tag for either a monad or a string or whatever
-- just makes syntax better, and ideally in the future, everything better.
class Printable a where printThis :: a -> HtmlWriter ()

instance Printable [Char] where 
  printThis = writeString 

instance Printable (HtmlWriter a) where 
  printThis = void
    
instance Printable (Param a) => Printable (Elem (Param a)) where
  printThis (Elem tg msg) = do  
    writeString $ "<"++tg++" "
    printThis msg
    writeString $ "</"++tg++">\n"

printElem (Elem tg msg) = do  
    writeString $ "<"++tg++">\n"
    printThis msg
    writeString $ "</"++tg++">\n"
    
instance Printable (Elem String) where printThis = printElem
instance (Printable (Elem a)) => Printable (Elem (Elem a)) where printThis = printElem
instance (Printable (HtmlWriter a)) => Printable (Elem (HtmlWriter a)) where printThis = printElem

instance (Printable (Param a)) => Printable (Param (Param a)) where
  printThis (Param params msg) = do
    printThis $ " "++showMiddle params++" "
    printThis msg
    
printParam (Param params msg) = do
  printThis $ " "++showMiddle params++">\n"
  printThis msg

instance Printable (Param String) where printThis = printParam
instance (Printable (HtmlWriter a)) => Printable (Param (HtmlWriter a)) where printThis = printParam
instance (Printable (Elem a)) => Printable (Param (Elem a)) where printThis = printParam

showMiddle a = reverse $ tail $ reverse $ tail $ show a

tag tg = printThis . Elem tg

link lk params msg = anchor $ Param ([Attr "href" $ show lk]++params) msg

href :: String -> HtmlAttr
href ln = Attr "href" (show ln)

name :: String -> HtmlAttr
name ln = Attr "id" (show ln)

script = appE [| tag "script" |] . hsToJs


-- HTML tags
address             =  tag "ADDRESS"
anchor              =  tag "A"
applet              =  tag "APPLET"
big                 =  tag "BIG"
blockquote          =  tag "BLOCKQUOTE"
body                =  tag "BODY"
bold                =  tag "BOLD"
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



