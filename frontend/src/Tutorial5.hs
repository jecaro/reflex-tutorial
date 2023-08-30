{-# LANGUAGE OverloadedStrings #-}

module Tutorial5 (tutorial5) where

import Data.Text (pack, unpack)
import Reflex.Dom.Core
import Text.Read (readMaybe)
import Utils (inputClasses, oneColumnClasses)

tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = elAttr "div" ("class" =: oneColumnClasses) $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  el "div" $ dynText numberString
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
