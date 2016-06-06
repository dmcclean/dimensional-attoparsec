{-# LANGUAGE RankNTypes #-}

module Numeric.Units.Dimensional.Parsing.Units where

import Control.Applicative
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

data LanguageDefinition = LanguageDefinition 
                          { units :: [AnyUnit]
                          , unaryFunctions :: Map.Map String (DynQuantity ExactPi -> DynQuantity ExactPi)
                          , constants :: Map.Map String (DynQuantity ExactPi)
                          , allowPrefixPositiveSign :: Bool
                          , allowSuperscriptExponentiation :: Bool
                          }

defaultConstants :: Map.Map String (DynQuantity ExactPi)
defaultConstants = Map.fromList [ ("pi",  demoteQuantity $ pi *~ one)
                                , ("tau", demoteQuantity $ (2 P.* pi) *~ one)
                                ]

defaultUnaryFunctions :: Map.Map String (DynQuantity ExactPi -> DynQuantity ExactPi)
defaultUnaryFunctions = Map.fromList [ ("abs", abs)
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

{-
Lexical Rules
-}
idStyle :: TokenParsing m => IdentifierStyle m
idStyle = IdentifierStyle
          { _styleName = "identifier"
          , _styleReserved =  fromList $ (Map.keys defaultUnaryFunctions) ++ ["pi", "tau"]
          , _styleStart = letter
          , _styleLetter = alphaNum <|> char '_'
          , _styleHighlight = Identifier
          , _styleReservedHighlight = ReservedIdentifier
          }

whiteSpace :: TokenParsing m => m ()
whiteSpace = Text.Parser.Token.whiteSpace

identifier :: (TokenParsing m, Monad m) => m Text
identifier = ident idStyle

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve idStyle

reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve emptyOps

{-
Expressions for Quantities
-}
expr :: (Monad m, TokenParsing m) => [AnyUnit] -> m (DynQuantity ExactPi)
expr us = buildExpressionParser table (term us)
      <?> "expression"

term :: (Monad m, TokenParsing m) => [AnyUnit] -> m (DynQuantity ExactPi)
term us = parens (expr us)
      <|> unaryFunctionApplication us
      <|> quantity us
      <?> "simple expression"

table :: (Monad m, TokenParsing m) => [[Operator m (DynQuantity ExactPi)]]
table = [ [preop "-" negate, preop "+" id ]
        , [binop "^" exponentiation AssocRight]
        , [binop "*" (P.*) AssocLeft, binop "/" (P./) AssocLeft ]
        , [binop "+" (+) AssocLeft, binop "-" (-)   AssocLeft ]
        ]

binop :: (Monad m, TokenParsing m) => String -> (a -> a -> a) -> Assoc -> Operator m a
binop  name fun assoc = Infix (fun <$ reservedOp name) assoc

preop, postop :: (Monad m, TokenParsing m) => String -> (a -> a) -> Operator m a
preop  name fun       = Prefix (fun <$ reservedOp name)
postop name fun       = Postfix (fun <$ reservedOp name)

-- When we raise a dynamic quantity to an exact dimensionless integer power, we can use the more dimensionally-lenient ^ operator.
-- When the exponent does not meet these conditions we fall back to the ** operator.
exponentiation :: DynQuantity ExactPi -> DynQuantity ExactPi -> DynQuantity ExactPi
exponentiation x y | Just y' <- y /~ demoteUnit' one, Just y'' <- toExactInteger y' = x P.^ y''
                   | otherwise = x ** y

unaryFunctionApplication :: (TokenParsing m, Monad m) => [AnyUnit] -> m (DynQuantity ExactPi)
unaryFunctionApplication us = unaryFunction <*> parens q
  where
    q = expr us

unaryFunction :: (TokenParsing m, Monad m) => m (DynQuantity ExactPi -> DynQuantity ExactPi)
unaryFunction = choice $ fmap (\(n, f) -> f <$ reserved n) (Map.toList defaultUnaryFunctions)

quantity :: (TokenParsing m, Monad m) => [AnyUnit] -> m (DynQuantity ExactPi)
quantity us = wrap <$> numberWithPowers <*> option (Just $ demoteUnit' one) (unit us)
  where
    wrap :: ExactPi -> Maybe AnyUnit -> DynQuantity ExactPi
    wrap x (Just u) = x Dyn.*~ u
    wrap _ Nothing  = invalidQuantity

numberWithPowers :: (TokenParsing m, Monad m) => m ExactPi
numberWithPowers = token $ applyPowers <$> numberWithSuperscriptPower <*> many (symbolic '^' *> sign <*> decimal)
  where
    numberWithSuperscriptPower = power <$> number <*> optional superscriptInteger
    applyPowers :: ExactPi -> [Integer] -> ExactPi
    applyPowers x (n:ns) = (applyPowers x ns) ^^ n
    applyPowers x _ = x
    power :: ExactPi -> Maybe Integer -> ExactPi
    power x (Just n) = x ^^ n
    power x _ = x

unit :: (TokenParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
unit us = prod <$> some oneUnit
  where
    prod :: [Maybe AnyUnit] -> Maybe AnyUnit
    prod = F.foldl' (liftA2 (Dyn.*)) (Just $ (demoteUnit' one))
    oneUnit :: (TokenParsing m, Monad m) => m (Maybe AnyUnit)
    oneUnit = token $ applyPowers <$> unitWithSuperscriptPower <*> many (symbolic '^' *> sign <*> decimal)
    applyPowers :: Maybe AnyUnit -> [Integer] -> Maybe AnyUnit
    applyPowers u (n:ns) = power (applyPowers u ns) (Just n)
    applyPowers u _ = u
    unitWithSuperscriptPower :: (TokenParsing m, Monad m) => m (Maybe AnyUnit)
    unitWithSuperscriptPower = power <$> bareUnit us <*> optional superscriptInteger
    power :: Maybe AnyUnit -> Maybe Integer -> Maybe AnyUnit
    power (Just u) (Just n) = Just $ u Dyn.^ n
    power u _ = u

bareUnit :: (CharParsing m, Monad m) => [AnyUnit] -> m (Maybe AnyUnit)
bareUnit us = try (Just <$> fullAtomicUnit us)
          <|> try (Just <$> abbreviatedAtomicUnit us)
          <|> try (prefixedFullUnit us)
          <|> try (prefixedAtomicUnit us)

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
                    return $ u <$ (string . f $ a) <* notFollowedBy letter

abbreviatedPrefix :: (CharParsing m, Monad m) => m Prefix
abbreviatedPrefix = prefix abbreviation_en

fullPrefix :: (CharParsing m, Monad m) => m Prefix
fullPrefix = prefix name_en

prefix :: (CharParsing m, Monad m) => (PrefixName -> String) -> m Prefix
prefix f = choice $ fmap parsePrefix siPrefixes
  where
    parsePrefix :: (CharParsing m, Monad m) => Prefix -> m Prefix
    parsePrefix p = p <$ (string . f . prefixName $ p)

sign :: (TokenParsing m, Num a) => m (a -> a)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

number :: (TokenParsing m, Monad m) => m ExactPi
number = pi <$ reserved "pi"
     <|> 2 P.* pi <$ reserved "tau"
     <|> convert <$> naturalOrScientific
  where
    convert (Left x) = realToFrac x
    convert (Right x) = realToFrac x

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
