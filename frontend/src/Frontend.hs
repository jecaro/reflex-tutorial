{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Common.Route
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Obelisk Minimal Example"
        elAttr
          "script"
          ( "type" =: "application/javascript"
              <> "src" =: $(static "lib.js")
          )
          blank
        elAttr
          "link"
          ( "href" =: $(static "main.css")
              <> "type" =: "text/css"
              <> "rel" =: "stylesheet"
          )
          blank,
      _frontend_body = do
        el "h1" $ text "Welcome to Obelisk!"

        tutorial1
    }

tutorial1 :: DomBuilder t m => m ()
tutorial1 = el "div" $ text "Welcome to Reflex"
