{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Numeric.Units.Dimensional.Parsing.Attoparsec
(
  ucumUnit
, ucumSI
, buildUnitMap
, allUcumUnits
, atomSymbol
, atom
, term
, annotation
, bracket
)
where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.ExactPi as E
import Data.Map as M
import Data.Text as T
import Numeric.Units.Dimensional as D hiding (recip)
import Numeric.Units.Dimensional.Dynamic
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.UnitNames as N hiding (atom)
import Numeric.Units.Dimensional.UnitNames.InterchangeNames as I
import Prelude hiding (exponent, recip)
import qualified Prelude as P

type AnyValue = (ExactPi, AnyUnit)

type UnitMap = Map Text AnyUnit

buildUnitMap :: [AnyUnit] -> Either String UnitMap
buildUnitMap [] = return M.empty
buildUnitMap (u:us) = do
                        if I.isAtomic n
                          then buildUnitMap us >>= Right . M.insert (T.pack . I.name $ n) u
                          else Left "Only atomic units may be supplied."
  where
    n = interchangeName u

allUcumUnits :: [AnyUnit]
allUcumUnits = [ demoteUnit metre
               , demoteUnit gram
               , demoteUnit second
               , demoteUnit ampere
               , demoteUnit kelvin
               , demoteUnit mole
               , demoteUnit candela
               , demoteUnit radian
               , demoteUnit steradian
               , demoteUnit hertz
               , demoteUnit newton
               , demoteUnit pascal
               , demoteUnit joule
               , demoteUnit watt
               , demoteUnit coulomb
               , demoteUnit volt
               , demoteUnit farad
               , demoteUnit ohm
               , demoteUnit siemens
               , demoteUnit weber
               , demoteUnit tesla
               , demoteUnit henry
               , demoteUnit lumen
               , demoteUnit lux
               , demoteUnit degreeCelsius
               , demoteUnit becquerel
               , demoteUnit gray
               , demoteUnit sievert
               , demoteUnit katal
               , demoteUnit degree
               , demoteUnit arcminute
               , demoteUnit arcsecond
               , demoteUnit degreeOfArc
               , demoteUnit minuteOfArc
               , demoteUnit secondOfArc
               , demoteUnit astronomicalUnit
               ]

ucumSI :: Parser AnyValue
ucumSI = ucumUnit allUcumUnits

ucumUnit :: [AnyUnit] -> Parser AnyValue
ucumUnit us = do
                let m = buildUnitMap us
                case m of
                  Left err -> fail $ "Unable to build unit map. " ++ err
                  Right m' -> let term' = term m'
                               in recip <$> (char '/' *> term')
                                  <|> term'

factor :: Parser Integer
factor = decimal

exponent :: Parser Integer
exponent = signed decimal

atomSymbol :: Parser Text
atomSymbol = do
               r <- core
               if (P.all (inClass "0-9") . T.unpack $ r)
                 then fail "Expected an atom symbol but all characters were digits."
                 else return r
  where
    core = A.takeWhile1 $ inClass "!#-'*,0-<>-Z\\^-z|~"

atom :: UnitMap -> Parser AnyUnit
atom m = do
           sym <- atomSymbol
           case M.lookup sym m of
             Just u  -> return u
             Nothing -> fail $ "Unrecognized atom symbol " ++ T.unpack sym

prefix :: Parser Prefix
prefix = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: Prefix -> Parser Prefix
    parsePrefix p = p <$ (string . T.pack . I.name . interchangeName $ p)

simpleUnit :: UnitMap -> Parser AnyUnit
simpleUnit m = (prefixed a) <|> a
  where
    a = atom m

prefixed :: Parser AnyUnit -> Parser AnyUnit
prefixed a = do
               p <- prefix
               u <- a
               -- dynamically verify that the AnyUnit was metric, then apply the prefix and return it
               -- if it wasn't metric, return an error
               error "monkey"

annotatable :: UnitMap -> Parser AnyUnit
annotatable m = simpleUnitWithExponent su <|> su
  where
    su = simpleUnit m
    simpleUnitWithExponent :: Parser AnyUnit -> Parser AnyUnit
    simpleUnitWithExponent s = do
                                 u <- s
                                 e <- exponent
                                 -- dynamically apply the exponent
                                 undefined

annotation :: Parser Text
annotation = bracket '{' '}' $ A.takeWhile (\c -> '!' <= c && c <= '~' && c /= '{' && c /= '}')

term :: UnitMap -> Parser AnyValue
term m = term'
  where
    term' = choice [ multiply <$> component <*> (char '.' *> term')
                   , divide   <$> component <*> (char '/' *> term')
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
    ann = annotatable m

recip :: AnyValue -> AnyValue
recip (x, u) = (P.recip x, error "frog")

multiply :: AnyValue -> AnyValue -> AnyValue
multiply (x1, u1) (x2, u2) = (x1 P.* x2, error "squirrel")

divide :: AnyValue -> AnyValue -> AnyValue
divide (x1, u1) (x2, u2)   = (x1 P./ x2, error "moose")

bracket :: Char -> Char -> Parser a -> Parser a
bracket b e p = char b *> p <* char e
