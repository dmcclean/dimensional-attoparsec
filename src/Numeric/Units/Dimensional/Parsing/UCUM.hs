{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Numeric.Units.Dimensional.Parsing.UCUM
(
  ucumUnit
, ucumSI
, buildUnitMap
, allUcumUnits
, allSIUnits
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
import Data.Maybe (fromJust)
import Data.Text as T
import Numeric.Units.Dimensional.Dynamic hiding ((*), (/), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.UnitNames
import Prelude hiding (exponent, recip)
import qualified Prelude as P

type AnyValue = (ExactPi, Maybe AnyUnit) -- second component of Nothing identifies an implicit unit of 1 which should not appear unless required

type UnitMap = Map Text AnyUnit

buildUnitMap :: [AnyUnit] -> Either String UnitMap
buildUnitMap [] = return M.empty
buildUnitMap (u:us) = do
                        case asAtomic n of
                          Just n' -> buildUnitMap us >>= Right . M.insert (T.pack . fromJust . nameComponent ucum $ n') u
                          Nothing -> Left "Only atomic units may be supplied."
  where
    n = name u

allUcumUnits :: [AnyUnit]
allUcumUnits = allSIUnits ++
  [ demoteUnit' electronVolt
  , demoteUnit' unifiedAtomicMassUnit
  , demoteUnit' gee
  , demoteUnit' inch
  , demoteUnit' foot
  , demoteUnit' mil
  , demoteUnit' poundForce
  -- , demoteUnit' horsepower
  , demoteUnit' psi
  , demoteUnit' yard
  , demoteUnit' mile
  , demoteUnit' nauticalMile
  , demoteUnit' knot
  , demoteUnit' teaspoon
  , demoteUnit' acre
  , demoteUnit' year
  , demoteUnit' bar
  , demoteUnit' atmosphere
  , demoteUnit' technicalAtmosphere
  -- , demoteUnit' mHg
  , demoteUnit' inHg_UCUM
  , demoteUnit' rad
  , demoteUnit' stokes
  , demoteUnit' imperialGallon
  , demoteUnit' imperialQuart
  , demoteUnit' imperialPint
  , demoteUnit' imperialCup
  , demoteUnit' imperialGill
  , demoteUnit' imperialFluidOunce
  , demoteUnit' usGallon
  , demoteUnit' usQuart
  , demoteUnit' usPint
  , demoteUnit' usCup
  , demoteUnit' usGill
  , demoteUnit' usFluidOunce
  ]

allSIUnits :: [AnyUnit]
allSIUnits =
  [ demoteUnit' metre
  , demoteUnit' gram
  , demoteUnit' second
  , demoteUnit' ampere
  , demoteUnit' kelvin
  , demoteUnit' mole
  , demoteUnit' candela
  , demoteUnit' radian
  , demoteUnit' steradian
  , demoteUnit' hertz
  , demoteUnit' newton
  , demoteUnit' pascal
  , demoteUnit' joule
  , demoteUnit' watt
  , demoteUnit' coulomb
  , demoteUnit' volt
  , demoteUnit' farad
  , demoteUnit' ohm
  , demoteUnit' siemens
  , demoteUnit' weber
  , demoteUnit' tesla
  , demoteUnit' henry
  , demoteUnit' lumen
  , demoteUnit' lux
  --, demoteUnit' degreeCelsius -- TODO: add this back once degreeCelsius has a proper unit name
  , demoteUnit' becquerel
  , demoteUnit' gray
  , demoteUnit' sievert
  , demoteUnit' katal
  , demoteUnit' degree
  , demoteUnit' arcminute
  , demoteUnit' arcsecond
  , demoteUnit' astronomicalUnit
  , demoteUnit' liter
  , demoteUnit' hectare
  , demoteUnit' tonne
  , demoteUnit' minute
  , demoteUnit' hour
  , demoteUnit' day
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
atomSymbol = (\a -> T.concat ["[", a, "]"]) <$> bracket '[' ']' atomSymbolCore
         <|> atomSymbolCore
  where
    atomSymbolCore = do
                       r <- core
                       if (P.all (inClass "0-9") . T.unpack $ r)
                         then fail "Expected an atom symbol but all characters were digits."
                         else return r
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
    parsePrefix p = p <$ (string . T.pack . fromJust . nameComponent ucum . prefixName $ p)

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
               case Dyn.applyPrefix p u of
                 Just u' -> return u'
                 Nothing -> fail "Metric prefix was applied to non-metric unit."

annotatable :: UnitMap -> Parser AnyUnit
annotatable m = simpleUnitWithExponent su <|> su
  where
    su = simpleUnit m
    simpleUnitWithExponent :: Parser AnyUnit -> Parser AnyUnit
    simpleUnitWithExponent s = do
                                 u <- s
                                 e <- exponent
                                 return $ u Dyn.^ e

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
    component = choice [ justUnit <$> ann <* annotation
                       , justUnit <$> ann
                       , (const (1, Nothing)) <$> annotation
                       , ((, Nothing) . fromInteger) <$> factor
                       , bracket '(' ')' $ term'
                       ]
    justUnit = (1,) . Just
    ann = annotatable m

recip :: AnyValue -> AnyValue
recip (x, Nothing) = (P.recip x, Nothing)
recip (x, Just u)  = (P.recip x, Just $ Dyn.recip u)

liftAV2 :: (ExactPi -> ExactPi -> ExactPi) -> (AnyUnit -> AnyUnit -> AnyUnit) -> AnyValue -> AnyValue -> AnyValue
liftAV2 fv _  (x1, u1)      (x2, Nothing) = (fv x1 x2, u1)
liftAV2 fv _  (x1, Nothing) (x2, u2)      = (fv x1 x2, u2)
liftAV2 fv fu (x1, Just u1) (x2, Just u2) = (fv x1 x2, Just $ fu u1 u2)

multiply :: AnyValue -> AnyValue -> AnyValue
multiply = liftAV2 (P.*) (Dyn.*)

divide :: AnyValue -> AnyValue -> AnyValue
divide = liftAV2 (P./) (Dyn./)

bracket :: Char -> Char -> Parser a -> Parser a
bracket b e p = char b *> p <* char e
