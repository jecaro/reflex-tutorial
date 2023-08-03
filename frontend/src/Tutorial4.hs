{-# LANGUAGE OverloadedStrings #-}

module Tutorial4 (tutorial4) where

import Reflex.Dom.Core

tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = el "div" $ do
  t <-
    inputElement $
      def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes
          .~ ("type" =: "number")
  text " "
  dynText $ _inputElement_value t
