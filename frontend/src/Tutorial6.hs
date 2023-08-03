{-# LANGUAGE OverloadedStrings #-}

module Tutorial6 (tutorial6) where

import Data.Text (pack, unpack)
import Reflex.Dom.Core
import Text.Read

tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = el "div" $ do
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = "
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (pack . show) result
  dynText resultString
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
