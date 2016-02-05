{-# LANGUAGE RankNTypes #-}

module Numeric.Units.Dimensional.Parsing.Units where

import Control.Applicative
import Data.ExactPi as E
import qualified Data.Foldable as F
import Data.HashSet (fromList)
import Data.Maybe (mapMaybe)
import Data.Text as T
import Numeric.Units.Dimensional.Dynamic hiding ((*), (/), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.UnitNames
import Prelude hiding (exponent, recip)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Prelude as P

idStyle :: TokenParsing m => IdentifierStyle m
idStyle = IdentifierStyle
          { _styleName = "identifier"
          , _styleReserved =  fromList $ (fmap fst unaryFunctions)
          , _styleStart = letter
          , _styleLetter = alphaNum <|> char '_'
          , _styleHighlight = Identifier
          , _styleReservedHighlight = ReservedIdentifier
          }

identifier :: (TokenParsing m, Monad m) => m Text
identifier = ident idStyle

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve idStyle

quantity :: (TokenParsing m, Monad m) => [AnyUnit] -> m (DynQuantity ExactPi)
quantity us = try $ unitAsQuantity us
          <|> unaryFunctionApplication us

unaryFunctionApplication :: (TokenParsing m, Monad m) => [AnyUnit] -> m (DynQuantity ExactPi)
unaryFunctionApplication us = unaryFunction <*> parens q
  where
    q = quantity us

unaryFunction :: (TokenParsing m, Monad m) => m (DynQuantity ExactPi -> DynQuantity ExactPi)
unaryFunction = choice $ fmap (\(n, f) -> f <$ reserved n) unaryFunctions

unaryFunctions :: [(String, DynQuantity ExactPi -> DynQuantity ExactPi)]
unaryFunctions = [ ("abs", abs)
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

unitAsQuantity :: (TokenParsing m, Monad m) => [AnyUnit] -> m (DynQuantity ExactPi)
unitAsQuantity us = wrap <$> unit us
  where
    wrap (Just u) = 1 *~ u
    wrap Nothing  = invalidQuantity

unit :: (TokenParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
unit us = token $ power <$> bareUnit us <*> optional superscriptInteger
  where
    power :: Maybe AnyUnit -> Maybe Integer -> Maybe AnyUnit
    power (Just u) (Just n) = Just $ u Dyn.^ n
    power u _ = u

bareUnit :: (CharParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
bareUnit us = try (Just <$> fullAtomicUnit us)
          <|> try (Just <$> abbreviatedAtomicUnit us)
          <|> try (prefixedFullUnit us)
          <|> prefixedAtomicUnit us

prefixedFullUnit :: (CharParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
prefixedFullUnit us = Dyn.applyPrefix <$> fullPrefix <*> fullAtomicUnit us

prefixedAtomicUnit :: (CharParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
prefixedAtomicUnit us = Dyn.applyPrefix <$> abbreviatedPrefix <*> abbreviatedAtomicUnit us

abbreviatedAtomicUnit :: (CharParsing m) => [AnyUnit] -> m AnyUnit
abbreviatedAtomicUnit = atomicUnit abbreviation_en

fullAtomicUnit :: (CharParsing m) => [AnyUnit] -> m AnyUnit
fullAtomicUnit = atomicUnit name_en

atomicUnit :: (CharParsing m) => (forall a.NameAtom a -> String) -> [AnyUnit] -> m AnyUnit
atomicUnit f us = choice $ mapMaybe parseUnit us
  where
    parseUnit :: (CharParsing m) => AnyUnit -> Maybe (m AnyUnit)
    parseUnit u = do
                    let n = anyUnitName u
                    a <- asAtomic n
                    return $ u <$ (string . f $ a)

abbreviatedPrefix :: (CharParsing m, Monad m) => m Prefix
abbreviatedPrefix = prefix abbreviation_en

fullPrefix :: (CharParsing m, Monad m) => m Prefix
fullPrefix = prefix name_en

prefix :: (CharParsing m, Monad m) => (PrefixName -> String) -> m Prefix
prefix f = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: (CharParsing m, Monad m) => Prefix -> m Prefix
    parsePrefix p = p <$ (string . f . prefixName $ p)

integerExponent :: (TokenParsing m) => m Integer
integerExponent = symbolic '^' *> sign <*> decimal

sign :: (TokenParsing m, Num a) => m (a -> a)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

superscriptInteger :: (CharParsing m, Integral a) => m a
superscriptInteger = superscriptSign <*> superscriptDecimal

superscriptSign :: (CharParsing m, Num a) => m (a -> a)
superscriptSign = negate <$ char '\x207b'
              <|> id <$ char '\x207a'
              <|> pure id

superscriptDecimal :: (CharParsing m, Num a) => m a
superscriptDecimal = F.foldl' (\x d -> (10 P.* x) P.+ d) 0 <$> some superscriptDigit

superscriptDigit :: (CharParsing m, Num a) => m a
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
