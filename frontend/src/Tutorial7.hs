{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Tutorial7 (Op (..), tutorial7) where

import Control.Monad.Fix
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Reflex.Dom.Core
import Text.Read

data Op = Plus | Minus | Times | Divide
  deriving stock (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp Plus = (+)
runOp Minus = (-)
runOp Times = (*)
runOp Divide = (/)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  op <- _dropdown_value <$> dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) op values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
              .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
