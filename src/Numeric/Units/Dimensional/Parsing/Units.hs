{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.Units.Dimensional.Parsing.Units
(
  -- * Parsers
  expression
, quantity
, unit
, number
  -- * Customizing the Accepted Language
, LanguageDefinition(..)
, defaultLanguageDefinition
)
where

import Control.Applicative
import Control.Monad.Reader
import Data.ExactPi as E
import qualified Data.Foldable as F
import Data.HashSet (fromList)
import Data.Maybe (mapMaybe)
import Data.Text as T
import qualified Data.Map as Map
import Numeric.Units.Dimensional (one, (*~))
import Numeric.Units.Dimensional.Dynamic hiding ((*~), (*), (/), recip)
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.UnitNames
import Prelude hiding (exponent, recip)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style (emptyOps)
import Text.Parser.Token.Highlight
import qualified Prelude as P

-- | A group of parameters defining a langauge of unit and quantity expressions.
data LanguageDefinition = LanguageDefinition 
                        { units :: [AnyUnit] -- ^ A list of units which may appear in 'quantity' or 'unit' expressions in the language.
                        , constants :: Map.Map Text (AnyQuantity ExactPi) -- ^ A list of constants which may appear in 'quantity' expressions in the language.
                        , unaryFunctions :: Map.Map Text (DynQuantity ExactPi -> DynQuantity ExactPi) -- ^ A list of unary functions which may be applied in 'expression's in the language.
                        , allowSuperscriptExponentiation :: Bool -- ^ 'True' if Unicode superscript exponents are to be permitted in the language.
                        }

defaultLanguageDefinition :: LanguageDefinition
defaultLanguageDefinition = LanguageDefinition
                          { units = []
                          , constants = Map.fromList [ ("pi",  demoteQuantity $ pi *~ one)
                                                     , ("tau", demoteQuantity $ (2 P.* pi) *~ one)
                                                     ]
                          , unaryFunctions = Map.fromList [ ("abs", abs)
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
                          , allowSuperscriptExponentiation = True
                          }

-- | An expression may be made by combining 'quantity's with the arithmetic operators ^, *, /, +, -,
-- prefix negation, and any 'unaryFunctions' available in the supplied 'LanguageDefinition'.
expression :: (TokenParsing m, MonadReader LanguageDefinition m) => m (DynQuantity ExactPi)
expression = buildExpressionParser table term
         <?> "expression"

term :: (MonadReader LanguageDefinition m, TokenParsing m) => m (DynQuantity ExactPi)
term = parens expression
   <|> unaryFunctionApplication
   <|> quantity
   <?> "simple expression"

table :: (Monad m, TokenParsing m) => [[Operator m (DynQuantity ExactPi)]]
table = [ [preop "-" negate ]
        , [binop "^" exponentiation AssocRight]
        , [binop "*" (P.*) AssocLeft, binop "/" (P./) AssocLeft ]
        , [binop "+" (+) AssocLeft, binop "-" (-)   AssocLeft ]
        ]

binop :: (Monad m, TokenParsing m) => Text -> (a -> a -> a) -> Assoc -> Operator m a
binop name fun assoc = Infix (fun <$ reservedOp name) assoc

preop :: (Monad m, TokenParsing m) => Text -> (a -> a) -> Operator m a
preop name fun = Prefix (fun <$ reservedOp name)

-- When we raise a dynamic quantity to an exact dimensionless integer power, we can use the more dimensionally-lenient ^ operator.
-- When the exponent does not meet these conditions we fall back to the ** operator.
exponentiation :: DynQuantity ExactPi -> DynQuantity ExactPi -> DynQuantity ExactPi
exponentiation x y | Just y' <- y /~ demoteUnit' one, Just y'' <- toExactInteger y' = x P.^ y''
                   | otherwise = x ** y

unaryFunctionApplication :: (TokenParsing m, MonadReader LanguageDefinition m) => m (DynQuantity ExactPi)
unaryFunctionApplication = unaryFunction <*> parens expression

unaryFunction :: (TokenParsing m, MonadReader LanguageDefinition m) => m (DynQuantity ExactPi -> DynQuantity ExactPi)
unaryFunction = do
                  ufs <- asks unaryFunctions
                  choice $ fmap (\(n, f) -> f <$ reserved n) (Map.toList ufs)

-- | A quantity may be a 'number' with an optional 'unit' (the unit 'one' is otherwise implied), or it may be one
-- of the 'constants' available in the supplied 'LanguageDefinition'.
quantity :: (TokenParsing m, MonadReader LanguageDefinition m) => m (DynQuantity ExactPi)
quantity = constant
       <|> (Dyn.*~) <$> numberWithPowers <*> option (demoteUnit' one) unit

numberWithPowers :: (TokenParsing m, MonadReader LanguageDefinition m) => m ExactPi
numberWithPowers = token $ applyPowers <$> numberWithSuperscriptPower <*> many (symbolic '^' *> sign <*> decimal)
  where
    numberWithSuperscriptPower = do
                                   useSuperscript <- asks allowSuperscriptExponentiation
                                   if useSuperscript
                                     then power <$> number <*> optional superscriptInteger
                                     else number
    applyPowers :: ExactPi -> [Integer] -> ExactPi
    applyPowers x (n:ns) = (applyPowers x ns) ^^ n
    applyPowers x _ = x
    power :: ExactPi -> Maybe Integer -> ExactPi
    power x (Just n) = x ^^ n
    power x _ = x

unit :: (TokenParsing m, MonadReader LanguageDefinition m) => m AnyUnit
unit = prod <$> some oneUnit
  where
    prod :: [AnyUnit] -> AnyUnit
    prod = F.foldl' (Dyn.*) (demoteUnit' one)
    oneUnit :: (TokenParsing m, MonadReader LanguageDefinition m) => m AnyUnit
    oneUnit = token $ applyPowers <$> unitWithSuperscriptPower <*> many (symbolic '^' *> sign <*> decimal)
    applyPowers :: AnyUnit -> [Integer] -> AnyUnit
    applyPowers u (n:ns) = power (applyPowers u ns) (Just n)
    applyPowers u _ = u
    unitWithSuperscriptPower :: (TokenParsing m, MonadReader LanguageDefinition m) => m AnyUnit
    unitWithSuperscriptPower = power <$> bareUnit <*> optional superscriptInteger
    power :: AnyUnit -> Maybe Integer -> AnyUnit
    power u (Just n) = u Dyn.^ n
    power u _ = u

bareUnit :: (CharParsing m, MonadReader LanguageDefinition m) => m AnyUnit
bareUnit = try fullAtomicUnit
       <|> try abbreviatedAtomicUnit
       <|> try prefixedFullUnit
       <|> try prefixedAtomicUnit

tryApplyPrefix :: (Parsing m, Monad m) => m Prefix -> m AnyUnit -> m AnyUnit
tryApplyPrefix p u = do
                       p' <- p
                       u' <- u
                       case (Dyn.applyPrefix p' u') of
                         Nothing -> unexpected "Metric prefix applied to non-metric unit."
                         Just pu -> return pu

prefixedFullUnit :: (CharParsing m, MonadReader LanguageDefinition m) => m AnyUnit
prefixedFullUnit = tryApplyPrefix fullPrefix fullAtomicUnit

prefixedAtomicUnit :: (CharParsing m, MonadReader LanguageDefinition m) => m AnyUnit
prefixedAtomicUnit = tryApplyPrefix abbreviatedPrefix abbreviatedAtomicUnit

abbreviatedAtomicUnit :: (CharParsing m, MonadReader LanguageDefinition m) => m AnyUnit
abbreviatedAtomicUnit = atomicUnit (Just . abbreviation_en)

fullAtomicUnit :: (CharParsing m, MonadReader LanguageDefinition m) => m AnyUnit
fullAtomicUnit = atomicUnit (Just . name_en)

atomicUnit :: (CharParsing m, MonadReader LanguageDefinition m) => (forall m'.UnitName m' -> Maybe String) -> m AnyUnit
atomicUnit f = do
                 us <- asks units
                 choice $ mapMaybe parseUnit us
  where
    parseUnit :: (CharParsing m) => AnyUnit -> Maybe (m AnyUnit)
    parseUnit u = do
                    let n = anyUnitName u
                    n' <- f n
                    return $ u <$ (string n') <* notFollowedBy letter

abbreviatedPrefix :: (CharParsing m, Monad m) => m Prefix
abbreviatedPrefix = prefix prefixAbbreviationEnglish

fullPrefix :: (CharParsing m, Monad m) => m Prefix
fullPrefix = prefix prefixNameEnglish

prefix :: (CharParsing m, Monad m) => (Prefix -> String) -> m Prefix
prefix f = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: (CharParsing m, Monad m) => Prefix -> m Prefix
    parsePrefix p = p <$ (string . f $ p)

sign :: (TokenParsing m, Num a) => m (a -> a)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

number :: (TokenParsing m, MonadReader LanguageDefinition m) => m ExactPi
number = dimensionlessConstant
     <|> convert <$> naturalOrScientific
  where
    convert (Left x) = realToFrac x
    convert (Right x) = realToFrac x

dimensionlessConstant :: (TokenParsing m, MonadReader LanguageDefinition m) => m ExactPi
dimensionlessConstant = do
                          cs <- asks constants
                          let ps = fmap (uncurry makeConstant) . Map.toList $ Map.mapMaybe (Dyn./~ (demoteUnit' one)) cs
                          choice ps
  where
    makeConstant :: (TokenParsing m, MonadReader LanguageDefinition m) => Text -> ExactPi -> m ExactPi
    makeConstant n v = v <$ reserved n

constant :: (TokenParsing m, MonadReader LanguageDefinition m) => m (DynQuantity ExactPi)
constant = do
             cs <- asks constants
             let ps = fmap (uncurry makeConstant) $ Map.toList cs
             choice ps
  where
    makeConstant :: (TokenParsing m, MonadReader LanguageDefinition m) => Text -> AnyQuantity ExactPi -> m (DynQuantity ExactPi)
    makeConstant n v = (demoteQuantity v) <$ reserved n

{-
Lexical Rules for Identifiers
-}
idStyle :: (TokenParsing m, MonadReader LanguageDefinition m) => m (IdentifierStyle m)
idStyle = do
            ufs <- asks unaryFunctions
            return $ IdentifierStyle
                       { _styleName = "identifier"
                       , _styleReserved = fromList . fmap T.unpack $ (Map.keys ufs) ++ ["pi", "tau"]
                       , _styleStart = letter
                       , _styleLetter = alphaNum <|> char '_'
                       , _styleHighlight = Identifier
                       , _styleReservedHighlight = ReservedIdentifier
                       }

reserved :: (TokenParsing m, MonadReader LanguageDefinition m) => Text -> m ()
reserved name = do
                  s <- idStyle
                  reserveText s name

reservedOp :: (TokenParsing m, Monad m) => Text -> m ()
reservedOp = reserveText emptyOps

{-
Parsing Superscript Numbers
-}
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
