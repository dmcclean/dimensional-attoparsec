{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Monad.Reader
import Data.AEq
import Data.Text as T
import Data.Either (either, isLeft)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Numeric.Units.Dimensional.Parsing.Units (expr, whiteSpace, defaultLanguageDefinition, LanguageDefinition(..))
import Numeric.Units.Dimensional.Parsing.UCUM (allUcumUnits)
import Data.ExactPi
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic (DynQuantity, AnyQuantity, demoteQuantity)
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.Codata
import qualified Numeric.Units.Dimensional.Dynamic as Dyn
import qualified Prelude as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unit Parser" $ do
         describe "Correct Parses" $ do
           describe "Exact Quantities" $
             mapM_ (uncurry workingExact) workingExamples
           describe "Approximate Quantities" $
             mapM_ (uncurry workingApprox) workingApproximateExamples
           describe "Ill-Dimensioned With No Value" $
             mapM_ parsesButHasNoValue examplesWithNoValue
         describe "Incorrect Parses" $ do
           mapM_ doesNotParse examplesThatShouldNotParse

lang :: LanguageDefinition
lang = defaultLanguageDefinition { constants = Map.insert "c" (demoteQuantity speedOfLightInVacuum) $ constants defaultLanguageDefinition
                                 , units = allUcumUnits
                                 }

parse :: Text -> Either String (DynQuantity ExactPi)
parse = let e = runReaderT expr lang
         in parseOnly (whiteSpace *> e <* endOfInput)

workingApprox :: Text -> AnyQuantity Double -> Spec
workingApprox e v = it ("Correctly Parses " ++ show e ++ " with Approximate Value " ++ show v) $ do
                      parse e `shouldSatisfy` matchesOn (\a b -> a ~== approximateValue b) v

workingExact :: Text -> AnyQuantity ExactPi -> Spec
workingExact e v = it ("Correctly Parses " ++ show e) $ do
                     parse e `shouldSatisfy` matchesOn areExactlyEqual v

parsesButHasNoValue :: Text -> Spec
parsesButHasNoValue e = it ("Correctly Parses and Assigns No Value to " ++ show e) $ do
                          parse e `shouldSatisfy` either (const False) ((== Nothing) . Dyn.dynamicDimension)

doesNotParse :: Text -> Spec
doesNotParse e = it ("Does Not Parse " ++ show e) $ do
                   parse e `shouldSatisfy` isLeft

matchesOn :: (Floating a) => (a -> ExactPi -> Bool) -> AnyQuantity a -> (Either String (DynQuantity ExactPi)) -> Bool
matchesOn _ _ (Left _) = False
matchesOn f a (Right b) = fromMaybe False $ do
                            let d = dimension a
                            let u = Dyn.siUnit d
                            a' <- a Dyn./~ u
                            b' <- b Dyn./~ u
                            return $ f a' b'

dq :: (KnownDimension d) => Quantity d a -> AnyQuantity a
dq = demoteQuantity

workingExamples :: [(Text, AnyQuantity ExactPi)]
workingExamples = 
  [ ("1",                     dq$ 1 *~ one)
  , ("1 s",                   dq$ 1 *~ second)
  , ("1 kg",                  dq$ 1 *~ kilo gram)
  , ("1 kilogram",            dq$ 1 *~ kilo gram)
  , ("1 m²",                  dq$ 1 *~ square meter)
  , ("2 m²",                  dq$ 2 *~ square meter)
  , ("(2 m)^2",               dq$ 4 *~ square meter)
  , ("9 mm",                  dq$ 9 *~ milli meter)
  , ("1 m^2",                 dq$ 1 *~ square meter)
  , ("1 m^-2",                dq$ 1 *~ (meter ^ neg2))
  , ("1 s⁻¹",                 dq$ 1 *~ hertz)
  , ("1 s⁺¹",                 dq$ 1 *~ second)
  , ("1 m s^-1",              dq$ 1 *~ (meter / second))
  , (" 1 m s^-1 ",            dq$ 1 *~ (meter / second))
  , ("1 m s⁻¹",               dq$ 1 *~ (meter / second))
  , ("1 kg m second^-2",      dq$ 1 *~ (kilo gram * meter * second^neg2))
  , ("pi / 4",                dq$ pi / _4)
  , ("tau",                   dq$ tau)
  , ("371 kg * 10 m",         dq$ (371 *~ kilo gram) * (10 *~ meter))
  , ("1 km + 9 inch",         dq$ (1 *~ kilo meter) + (9 *~ inch))
  , ("12 mile - 7 yard",      dq$ (12 *~ mile) - (7 *~ yard))
  , ("-3 W",                  dq$ -3 *~ watt)
  , ("6.022e23",              dq$ 6.022e23 *~ one)
  , ("2^-15 radian",          dq$ ((2 P.^^ -15) *~ radian))
  , ("0.3 * c",               dq$ (0.3 *~ one) * speedOfLightInVacuum)
  ]

workingApproximateExamples :: [(Text, AnyQuantity Double)]
workingApproximateExamples = 
  [ ("sin(27 degree)",        dq$ 0.45399049974 *~ one)
  , ("3 inch + 9 m * cos(4)", dq$ -5.80659259 *~ meter)
  , ("0.37 AU + 9000 km",     dq$ 55360212159 *~ meter)
  , ("log(tau)",              dq$ 1.83787706641 *~ one)
  , ("sqrt(1 mile^2)",        dq$ 1.609344 *~ kilo meter)
  , ("sqrt(43)",              dq$ 6.5574385243 *~ one)
  , ("exp(1)",                dq$ 2.71828182846 *~ one)
  , ("abs(log(2^-7 degree))", dq$ 8.90025723 *~ one)
  ]

examplesWithNoValue :: [Text]
examplesWithNoValue =
  [ "sin(12 m)"
  , "1 ft + 3 A"
  , "1m - 1 kg"
  , "sqrt(15 s)"
  , "3 kft" -- because ft is not a metric unit
  ]

examplesThatShouldNotParse :: [Text]
examplesThatShouldNotParse =
  [ ""
  , "3 kgms^2"
  , "kg 3"
  , "foo(2 m)"
  ]
