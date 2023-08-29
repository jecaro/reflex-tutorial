{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Javascript (javascript) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Text (Text)
import Language.Javascript.JSaddle (js, js1, jsg, liftJSM)
import Reflex.Dom.Core

javascript :: (DomBuilder t m, Prerender t0 m, Prerender t m) => m ()
javascript = do
  el "div" $ do
    text "Example of using a javascript library in the static assets"

    prerender_ blank $
      void . liftJSM $ do
        jsg ("window" :: Text)
          ^. js ("skeleton_lib" :: Text)
          ^. js1 ("log" :: Text) ("Hello, World!" :: Text)
