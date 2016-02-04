{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.Units.Dimensional.Parsing.Units where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.ExactPi as E
import Data.Maybe (mapMaybe)
import Data.Text as T
import Numeric.Units.Dimensional.Dynamic hiding ((*), (/), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.UnitNames
import Prelude hiding (exponent, recip)
import qualified Prelude as P

quantity :: [AnyUnit] -> Parser (DynQuantity ExactPi)
quantity us = try $ unitAsQuantity us
          <|> unaryFunctionApplication us

unaryFunctionApplication :: [AnyUnit] -> Parser (DynQuantity ExactPi)
unaryFunctionApplication us = do
                                f <- unaryFunction
                                _ <- char '('
                                x <- q
                                _ <- char ')'
                                return $ f x
  where
    q = quantity us

unaryFunction :: Parser (DynQuantity ExactPi -> DynQuantity ExactPi)
unaryFunction = choice $ fmap (\(n, f) -> f <$ asciiCI n) fs
  where
    fs = [ ("abs", abs)
         , ("signum", signum)
         , ("sgn", signum)
         , ("exp", exp)
         , ("log", log)
         , ("ln", log)
         , ("sqrt", sqrt)
         , ("sin", sin)
         , ("cos", cos)
         , ("tan", tan)
         , ("asin", asin)
         , ("acos", acos)
         , ("atan", atan)
         , ("sinh", sinh)
         , ("cosh", cosh)
         , ("tanh", tanh)
         , ("asinh", asinh)
         , ("acosh", acosh)
         , ("atanh", atanh)
         ]

unitAsQuantity :: [AnyUnit] -> Parser (DynQuantity ExactPi)
unitAsQuantity us = wrap <$> unit us
  where
    wrap (Just u) = 1 *~ u
    wrap Nothing  = invalidQuantity

unit :: [AnyUnit] -> Parser (Maybe AnyUnit)
unit us = prefixedFullUnit us
      <|> prefixedAtomicUnit us

prefixedFullUnit :: [AnyUnit] -> Parser (Maybe AnyUnit)
prefixedFullUnit us = do
                        p <- optional fullPrefix
                        u <- fullAtomicUnit us
                        return $ case p of
                                   Just p' -> Dyn.applyPrefix p' u
                                   Nothing -> Just u

prefixedAtomicUnit :: [AnyUnit] -> Parser (Maybe AnyUnit)
prefixedAtomicUnit us = do
                          p <- optional abbreviatedPrefix
                          u <- abbreviatedAtomicUnit us
                          return $ case p of
                                     Just p' -> Dyn.applyPrefix p' u
                                     Nothing -> Just u

abbreviatedAtomicUnit :: [AnyUnit] -> Parser AnyUnit
abbreviatedAtomicUnit = atomicUnit abbreviation_en

fullAtomicUnit :: [AnyUnit] -> Parser AnyUnit
fullAtomicUnit = atomicUnit name_en

atomicUnit :: (forall a.NameAtom a -> String) -> [AnyUnit] -> Parser AnyUnit
atomicUnit f us = choice $ mapMaybe parseUnit us
  where
    parseUnit :: AnyUnit -> Maybe (Parser AnyUnit)
    parseUnit u = do
                    let n = anyUnitName u
                    a <- asAtomic n
                    return $ u <$ (asciiCI . T.pack . f $ a)

abbreviatedPrefix :: Parser Prefix
abbreviatedPrefix = prefix abbreviation_en

fullPrefix :: Parser Prefix
fullPrefix = prefix name_en

prefix :: (PrefixName -> String) -> Parser Prefix
prefix f = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: Prefix -> Parser Prefix
    parsePrefix p = p <$ (asciiCI . T.pack . f . prefixName $ p)

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
