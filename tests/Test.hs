{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.AEq
import Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Numeric.Units.Dimensional.Parsing.Units (expr)
import Numeric.Units.Dimensional.Parsing.UCUM (allUcumUnits)
import Data.ExactPi
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Dynamic (DynQuantity, AnyQuantity, demoteQuantity)
import Numeric.Units.Dimensional.NonSI
import qualified Numeric.Units.Dimensional.Dynamic as Dyn

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Unit Parser" $ do
         describe "Correct Parses" $ do
           describe "Exact Quantities" $
             mapM_ (uncurry workingExact) workingExamples
           describe "Approximate Quantities" $
             mapM_ (uncurry workingApprox) workingApproximateExamples

parse :: Text -> Either String (DynQuantity ExactPi)
parse = parseOnly (expr allUcumUnits <* endOfInput)

workingApprox :: Text -> AnyQuantity Double -> Spec
workingApprox e v = it ("Correctly Parses " ++ show e ++ " with Approximate Value " ++ show v) $ do
                      parse e `shouldSatisfy` matches v
  where
    matches :: AnyQuantity Double -> (Either String (DynQuantity ExactPi)) -> Bool
    matches _ (Left _)  = False
    matches a (Right b) = fromMaybe False $ do
                            let d = dimension a
                            let u = Dyn.siUnit d
                            a' <- a Dyn./~ u
                            b' <- b Dyn./~ u
                            return $ a' ~== approximateValue b'

workingExact :: Text -> AnyQuantity ExactPi -> Spec
workingExact e v = it ("Correctly Parses " ++ show e) $ do
                     parse e `shouldSatisfy` matches v
  where
    matches :: AnyQuantity ExactPi -> (Either String (DynQuantity ExactPi)) -> Bool
    matches _ (Left _)  = False
    matches a (Right b) = fromMaybe False $ do
                            let d = dimension a
                            let u = Dyn.siUnit d
                            a' <- a Dyn./~ u
                            b' <- b Dyn./~ u
                            return $ areExactlyEqual a' b'

    {-
    matches a (Right b) | da == db = fromMaybe False $ areExactlyEqual <$> (a Dyn./~ siUnit da) <$> (b Dyn./~ siUnit db)
      where
        da = dimension a
        db = dimension b
    }-}

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
  , ("1 kg m second^-2",      dq$ 1 *~ (kilo gram * meter * second^neg2))
  , ("pi / 4",                dq$ pi / _4)
  , ("tau",                   dq$ tau)
  , ("371 kg * 10 m",         dq$ (371 *~ kilo gram) * (10 *~ meter))
  , ("1 km + 9 inch",         dq$ (1 *~ kilo meter) + (9 *~ inch))
  , ("12 mile - 7 yard",      dq$ (12 *~ mile) - (7 *~ yard))
  , ("-3 W",                  dq$ -3 *~ watt)
  ]

workingApproximateExamples :: [(Text, AnyQuantity Double)]
workingApproximateExamples = 
  [ ("sin(27 degree)",        dq$ 0.45399049974 *~ one)
  , ("3 inch + 9 m * cos(4)", dq$ -5.80659259 *~ meter)
  , ("0.37 AU + 9000 km",     dq$ 55360212159 *~ meter)
  , ("log(tau)",              dq$ 1.83787706641 *~ one)
  ]
