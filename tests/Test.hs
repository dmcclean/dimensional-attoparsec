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

workingExamples :: [(Text, AnyQuantity ExactPi)]
workingExamples = 
  [ ("1", demoteQuantity $ 1 *~ one)
  , ("1 s", demoteQuantity $ 1 *~ second)
  , ("1 kg", demoteQuantity $ 1 *~ kilo gram)
  , ("1 kilogram", demoteQuantity $ 1 *~ kilo gram)
  , ("1 m²", demoteQuantity $ 1 *~ square meter)
  , ("1 m^2", demoteQuantity $ 1 *~ square meter)
  , ("1 s⁻¹", demoteQuantity $ 1 *~ hertz)
  , ("1 s⁺¹", demoteQuantity $ 1 *~ second)
  , ("pi / 4", demoteQuantity $ pi / _4)
  , ("tau", demoteQuantity $ tau)
  , ("371 kg * 10 m", demoteQuantity $ (371 *~ kilo gram) * (10 *~ meter))
  , ("1 km + 9 inch", demoteQuantity $ (1 *~ kilo meter) + (9 *~ inch))
  ]

workingApproximateExamples :: [(Text, AnyQuantity Double)]
workingApproximateExamples = 
  [ ("sin(27 degree)", demoteQuantity $ 0.45399049974 *~ one)
  , ("3 inch + 9 m * cos(4)", demoteQuantity $ -5.80659259 *~ meter)
  , ("0.37 AU + 9000 km", demoteQuantity $ 55360212159 *~ meter)
  , ("log(tau)", demoteQuantity $ 1.83787706641 *~ one)
  ]
