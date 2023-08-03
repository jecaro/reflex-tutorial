{-# LANGUAGE OverloadedStrings #-}

module Tutorial5 (tutorial5) where

import Data.Text (pack, unpack)
import Reflex.Dom.Core
import Text.Read

tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  text " "
  dynText numberString
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
