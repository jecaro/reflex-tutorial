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
import Tutorial1
import Tutorial10
import Tutorial11
import Tutorial2
import Tutorial3
import Tutorial4
import Tutorial5
import Tutorial6
import Tutorial7
import Tutorial8
import Tutorial9

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
        el "hr" blank
        tutorial2
        el "hr" blank
        tutorial3
        el "hr" blank
        tutorial4
        el "hr" blank
        tutorial5
        el "hr" blank
        tutorial6
        el "hr" blank
        tutorial7
        el "hr" blank
        tutorial8
        el "hr" blank
        tutorial9
        el "hr" blank
        tutorial10
        el "hr" blank
        tutorial11
    }
