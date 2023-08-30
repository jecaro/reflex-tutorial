{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Tutorial7 (Op (..), runOp, tutorial7) where

import Control.Monad.Fix (MonadFix)
import Data.Fixed (mod')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Reflex.Dom.Core
import Text.Read (readMaybe)
import Utils (inputClasses, oneColumnClasses)

data Op = Plus | Minus | Times | Divide | Modulo
  deriving stock (Eq, Ord, Show)

runOp :: Op -> Double -> Double -> Double
runOp Plus = (+)
runOp Minus = (-)
runOp Times = (*)
runOp Divide = (/)
runOp Modulo = mod'

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = elAttr "div" ("class" =: oneColumnClasses) $ do
  nx <- numberInput
  op <-
    _dropdown_value
      <$> dropdown
        Times
        (constDyn ops)
        (def & attributes .~ constDyn ("class" =: inputClasses))
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) op values
      resultText = fmap (pack . show) result
  el "div" $ text " = " >> dynText resultText
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & initialAttributes
              .~ ("type" =: "number" <> "class" =: inputClasses)
      pure . fmap (readMaybe . unpack) $ _inputElement_value n
