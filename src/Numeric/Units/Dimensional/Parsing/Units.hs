module Numeric.Units.Dimensional.Parsing.Units where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.ExactPi as E
import Data.Map as M
import Data.Text as T
import Numeric.Units.Dimensional.Dynamic hiding ((*), (/), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.UnitNames
import Prelude hiding (exponent, recip)
import qualified Prelude as P

abbreviatedPrefix :: Parser Prefix
abbreviatedPrefix = prefix undefined

fullPrefix :: Parser Prefix
fullPrefix = prefix undefined

prefix :: (PrefixName -> String) -> Parser Prefix
prefix f = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: Prefix -> Parser Prefix
    parsePrefix p = p <$ (string . T.pack . f . prefixName $ p)

integerExponent :: Parser Integer
integerExponent = char '^' *> signed decimal
              <|> superscriptSigned superscriptDecimal

superscriptSigned :: Num a => Parser a -> Parser a
superscriptSigned p = (negate <$> (char '\x207b' *> p))
                  <|> (char '\x207a' *> p)
                  <|> p

superscriptDecimal :: Integral a => Parser a
superscriptDecimal = superscriptDigit

superscriptDigit :: Integral a => Parser a
superscriptDigit = 1 <$ char '¹'
               <|> 2 <$ char '²'
               <|> 3 <$ char '³'
               <|> 4 <$ char '⁴'
               <|> 5 <$ char '⁵'
               <|> 6 <$ char '\x2076'
               <|> 7 <$ char '\x2077'
               <|> 8 <$ char '\x2078'
               <|> 9 <$ char '\x2079'
               <|> 0 <$ char '\x2070'
