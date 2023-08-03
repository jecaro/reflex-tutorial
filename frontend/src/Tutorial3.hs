{-# LANGUAGE OverloadedStrings #-}

module Tutorial3 (tutorial3) where

import Reflex.Dom.Core

tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  text " "
  dynText $ _inputElement_value t
