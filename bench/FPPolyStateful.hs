
module FPPolyStateful (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse.PolyStateful

data S = S Int Char

ws, open, close, ident, sexp, src :: Parser () S () ()
ws      = many_ $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(char '(') >> ws
close   = $(char ')') >> ws
ident   = some_ (satisfyASCII_ isLatinLetter) >> ws
sexp    = branch open (some_ sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src () (S 10 'c')

longw, longws :: Parser () S () ()
longw     = $(string "thisisalongkeyword")
longws    = some_ (longw >> ws) >> eof
runLongws = runParser longws () (S 10 'c')

numeral, comma, numcsv :: Parser () S () ()
numeral   = some_ (satisfyASCII_ isDigit) >> ws
comma     = $(char ',') >> ws
numcsv    = numeral >> many_ (comma >> numeral) >> eof
runNumcsv = runParser numcsv () (S 10 'c')
