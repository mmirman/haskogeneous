{-# LANGUAGE 
 NoMonomorphismRestriction
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Language.XmlHtml.HtmlTags
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Some tags for writing html in a pretty format in haskell.

-- TODO: Use the module redirect tequnique to exclude items we don't want.
module Language.XmlHtml.HtmlTags where 

import Language.XmlHtml.XmlWriter

link lk params msg = anchor $ Param ([Attr "href" $ show lk]++params) msg

href ln = Param [Attr "href" (show ln)]

name ln = Param [Attr "id" (show ln)]

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
