{-# LANGUAGE OverloadedStrings #-}

module Tutorial2 (tutorial2) where

import Reflex.Dom.Core

tutorial2 :: DomBuilder t m => m ()
tutorial2 = el "div" $ do
  el "p" $ text "Reflex is:"
  elAttr "ul" ("class" =: "list-disc list-inside") $ do
    el "li" $ text "Efficient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"
