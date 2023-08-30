{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Javascript (javascript) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Text (Text)
import Language.Javascript.JSaddle (js, js1, jsg, liftJSM)
import Reflex.Dom.Core

default (Text)

javascript :: (DomBuilder t m, Prerender t0 m, Prerender t m) => m ()
javascript =
  el "div" $ do
    text "Example of using a javascript library in the static assets"

    prerender_ blank . void . liftJSM $
      jsg "window" ^. js "skeleton_lib" . js1 "log" "Hello, World!"
