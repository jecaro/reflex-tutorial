{-# LANGUAGE OverloadedStrings #-}

module Tutorial1 (tutorial1) where

import Reflex.Dom.Core

tutorial1 :: DomBuilder t m => m ()
tutorial1 = el "div" $ text "Welcome to Reflex"
