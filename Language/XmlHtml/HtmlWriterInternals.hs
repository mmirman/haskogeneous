{-# LANGUAGE 
 FlexibleInstances, 
 NoMonomorphismRestriction,
 TemplateHaskell,
 GeneralizedNewtypeDeriving,
 FlexibleContexts,
 EmptyDataDecls,
 MultiParamTypeClasses,
 FunctionalDependencies,
 GADTs
 #-}

---------------------------------------------------------------------------
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
import Control.Monad.Trans
import Control.Monad.Writer.Strict (WriterT, tell, execWriterT)
import Control.Monad.Writer.Class (MonadWriter(..))
import HGene.JSCompiler.HaskellToJavaScript

void a = a >> return ()

-- --------------------------------------------------------------------------
-- Type definition for the HtmlWriter
newtype HtmlWriter a = HtmlWriter { getHtml :: WriterT String IO a}
                     deriving (Monad, MonadWriter String)

liftIO t = HtmlWriter $ lift t

-- | @'writeString' s@ writes a string to the html writer monad
writeString = tell

-- | @'makeHtml' s@ currently just converts this into a string
makeHtml = execWriterT . getHtml . printThis

data End
data Par

data Param a b where 
  Param :: Printable a b => [HtmlAttr] -> a -> Param a b

data Elem a b where 
  Elem :: Printable a b => String -> a -> Elem a b

data HtmlAttr = Attr String String

instance Show HtmlAttr where
  show (Attr a b) = a++"="++b

-- --------------------------------------------------------------------------
-- Printable allows us to use tag for either a monad or a string or whatever
-- just makes syntax better, and ideally in the future, everything better.
class Printable a b | a -> b where printThis :: a -> HtmlWriter ()

instance Printable [Char] End where
  printThis = writeString

instance Printable (HtmlWriter a) End where 
  printThis = void

instance Printable a Par => Printable (Elem a Par) End where
  printThis (Elem tg msg) = do  
    writeString $ "<"++tg
    printThis msg
    writeString $ "</"++tg++">\n"

instance Printable a End => Printable (Elem a End) End where 
  printThis (Elem tg msg) = do
    writeString $ "<"++tg++">\n"
    printThis msg
    writeString $ "</"++tg++">\n"
    
instance Printable a Par => Printable (Param a Par) Par where
  printThis (Param params msg) = do
    printThis $ showMiddle params
    printThis msg
    
instance Printable a End => Printable (Param a End) Par where
  printThis (Param params msg) = do
    printThis $ showMiddle params++">\n"
    printThis msg  
  
showMiddle = concatMap (\x -> " "++show x)

tag tg = printThis . Elem tg

link lk params msg = anchor $ Param ([Attr "href" $ show lk]++params) msg

href :: Printable a b => String -> a -> Param a b
href ln = Param [Attr "href" (show ln)]

name :: Printable a b => String -> a -> Param a b
name ln = Param [Attr "id" (show ln)]

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
