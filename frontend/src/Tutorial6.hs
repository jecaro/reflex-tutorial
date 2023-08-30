{-# LANGUAGE OverloadedStrings #-}

module Tutorial6 (tutorial6) where

import Data.Text (pack, unpack)
import Reflex.Dom.Core
import Text.Read (readMaybe)
import Utils (inputClasses, oneColumnClasses)

tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = elAttr "div" ("class" =: oneColumnClasses) $ do
  nx <- numberInput
  el "div" $ text " + "
  ny <- numberInput
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (pack . show) result
  el "div" $ text " = " >> dynText resultString
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
