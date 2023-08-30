{-# LANGUAGE OverloadedStrings #-}

module Tutorial3 (tutorial3) where

import Reflex.Dom.Core
import Utils (inputClasses, oneColumnClasses)

tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = elAttr "div" ("class" =: oneColumnClasses) $ do
  t <- inputElement $ def & initialAttributes .~ ("class" =: inputClasses)
  el "div" $ dynText $ _inputElement_value t
