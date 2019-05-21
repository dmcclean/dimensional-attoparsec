{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Numeric.Units.Dimensional.Parsing.Units2
(
  Parser
, ParsedUnitName
, topLevel
, lexeme
, symbol
, quantity
, unitName
, atomicUnitName
, decimal
, superscriptInteger
, superscriptDecimal
)
where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Numeric.Units.Dimensional.UnitNames (Metricality(..))
import Numeric.Units.Dimensional.UnitNames.Internal (UnitName'(..))
import qualified Numeric.Units.Dimensional.UnitNames as N
import qualified Data.Foldable as F
import Prelude hiding (exponent)

type Parser = Parsec Void Text

type ParsedUnitName = UnitName' 'NonMetric Text

sc :: Parser ()
sc = L.space space1 empty empty

topLevel :: Parser a -> Parser a
topLevel = between sc eof

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

decimal :: (Integral a) => Parser a
decimal = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parsing Numbers
optionalSign :: (Num a) => Parser (a -> a)
optionalSign = sign
           <|> pure id

sign :: (Num a) => Parser (a -> a)
sign = negate <$ char '-'
   <|> id <$ char '+'

-- Parsing Quantities
quantity :: Parser a -> Parser (a, Maybe ParsedUnitName)
quantity n = do
                n' <- n
                u <- optional unitName
                return (n', u)
          <?> "quantity"

-- Parsing Unit Names
unitName :: Parser ParsedUnitName
unitName = try (Quotient <$> numeratorUnitName <*> (symbol "/" *> simpleUnitName))
       <|> try numeratorUnitName
       <|> withPowers simpleUnitName
  where
    optionalQuotient n (Just d) = Quotient n d
    optionalQuotient n Nothing = n

numeratorUnitName :: Parser ParsedUnitName
numeratorUnitName = One <$ symbol "1"
                <|> productUnitName

simpleUnitName :: Parser ParsedUnitName
simpleUnitName = grouped unitName
             <|> withSuperscriptPower atomicUnitName

grouped :: Parser ParsedUnitName -> Parser ParsedUnitName
grouped u = N.grouped <$> between (symbol "(") (symbol ")") u

productUnitName :: Parser ParsedUnitName
productUnitName = N.product <$> some simpleUnitName

withPowers :: Parser ParsedUnitName -> Parser ParsedUnitName
withPowers p = applyPowers <$> withSuperscriptPower p <*> many exponent
  where
    exponent :: Parser Int
    exponent = symbol "^" *> optionalSign <*> decimal
           <?> "exponent"
    applyPowers :: ParsedUnitName -> [Int] -> ParsedUnitName
    applyPowers u (n:ns) = applyPowers (u N.^ n) ns
    applyPowers u _ = u

withSuperscriptPower :: Parser ParsedUnitName -> Parser ParsedUnitName
withSuperscriptPower p = lexeme $ optionalPower <$> p <*> optional superscriptInteger
  where
    optionalPower :: ParsedUnitName -> Maybe Int -> ParsedUnitName
    optionalPower u (Just n) = u N.^ n
    optionalPower u _ = u

atomicUnitName :: Parser ParsedUnitName
atomicUnitName = lexeme atomicUnitName'

atomicUnitName' :: Parser ParsedUnitName
atomicUnitName' = Atomic <$> unitNameAtom <?> "atomic unit name"

unitNameAtom :: Parser Text
unitNameAtom = pack <$> try (some (letterChar <|> char '°' <|> char '\'') >>= check) <?> "unit name atom"
  where
    reservedWords = ["and"]
    check x = if x `elem` reservedWords
      then fail $ "keyword " ++ show x ++ " cannot be a unit name"
      else return x

-- Parsing Superscript Numbers
superscriptInteger :: (Integral a) => Parser a
superscriptInteger = optionalSuperscriptSign <*> superscriptDecimal
                 <?> "superscript integer"

optionalSuperscriptSign :: (Num a) => Parser (a -> a)
optionalSuperscriptSign = superscriptSign
                      <|> pure id

superscriptSign :: (Num a) => Parser (a -> a)
superscriptSign = negate <$ char '\x207b'
              <|> id <$ char '\x207a'
              <?> "superscript sign"

superscriptDecimal :: (Num a) => Parser a
superscriptDecimal = F.foldl' (\x d -> (10 * x) + d) 0 <$> some superscriptDigit
                 <?> "superscript decimal"

superscriptDigit :: (Num a) => Parser a
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
               <?> "superscript digit"
