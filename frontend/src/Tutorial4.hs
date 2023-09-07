{-# LANGUAGE OverloadedStrings #-}

module Tutorial4 (tutorial4) where

import Reflex.Dom.Core
import Utils (inputClasses, oneColumnClasses)

tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = elAttr "div" ("class" =: oneColumnClasses) $ do
  t <-
    inputElement $
      def
        & inputElementConfig_initialValue .~ "0"
        & initialAttributes .~ ("type" =: "number" <> "class" =: inputClasses)
  el "div" . dynText $ _inputElement_value t
