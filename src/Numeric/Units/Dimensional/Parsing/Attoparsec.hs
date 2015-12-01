{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Numeric.Units.Dimensional.Parsing.Attoparsec
(
  ucumUnit
)
where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.ExactPi as E
import Data.Text as T
import Numeric.Units.Dimensional hiding (recip)
import Numeric.Units.Dimensional.Dynamic
import Numeric.Units.Dimensional.UnitNames as N hiding (atom)
import Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import Prelude hiding (exponent, recip)
import qualified Prelude as P

type AnyValue = (ExactPi, AnyUnit)

ucumUnit :: [AnyUnit] -> Parser AnyValue
ucumUnit us = recip <$> (char '/' *> term')
          <|> term'
  where
    term' = term us

factor :: Parser Integer
factor = decimal

exponent :: Parser Integer
exponent = signed decimal

atomSymbol :: Parser Text
atomSymbol = undefined

atom :: [AnyUnit] -> Parser AnyUnit
atom us = fmap (undefined us) atomSymbol

prefix :: Parser Prefix
prefix = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: Prefix -> Parser Prefix
    parsePrefix p = p <$ (string . T.pack . I.name . interchangeName $ p)

simpleUnit :: [AnyUnit] -> Parser AnyUnit
simpleUnit us = (prefixed a) <|> a
  where
    a = atom us

prefixed :: Parser AnyUnit -> Parser AnyUnit
prefixed a = do
               p <- prefix
               u <- a
               -- dynamically verify that the AnyUnit was metric, then apply the prefix and return it
               -- if it wasn't metric, return an error
               error "monkey"

annotatable :: [AnyUnit] -> Parser AnyUnit
annotatable us = simpleUnitWithExponent su <|> su
  where
    su = simpleUnit us
    simpleUnitWithExponent :: Parser AnyUnit -> Parser AnyUnit
    simpleUnitWithExponent s = do
                                 u <- s
                                 e <- exponent
                                 -- dynamically apply the exponent
                                 undefined

annotation :: Parser Text
annotation = bracket '{' '}' $ A.takeWhile (\c -> '!' <= c && c <= '~' && c /= '{' && c /= '}')

term :: [AnyUnit] -> Parser AnyValue
term us = term'
  where
    term' = choice [ multiply <$> term' <* char '.' *> component
                   , divide   <$> term' <* char '/' *> component
                   , component
                   ]
    component :: Parser AnyValue
    component = choice [ (1,) <$> ann <* annotation
                       , fmap (1,) ann
                       , fmap (const (1, one')) annotation
                       , fmap ((, one') . fromInteger) factor
                       , bracket '(' ')' $ term'
                       ]
    one' = demoteUnit (one :: Unit 'NonMetric DOne Integer)
    ann = annotatable us

recip :: AnyValue -> AnyValue
recip (x, u) = (P.recip x, undefined u)

multiply :: AnyValue -> AnyValue -> AnyValue
multiply (x1, u1) (x2, u2) = (x1 P.* x2, undefined u1 u2)

divide :: AnyValue -> AnyValue -> AnyValue
divide (x1, u1) (x2, u2)   = (x1 P./ x2, undefined u1 u2)

bracket :: Char -> Char -> Parser a -> Parser a
bracket b e p = char b *> p <* char e
