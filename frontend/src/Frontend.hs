{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Common.Route (FrontendRoute (..))
import Home (home)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (subRoute_)
import Reflex.Dom.Core
import Tutorial1 (tutorial1)
import Tutorial10 (tutorial10)
import Tutorial11 (tutorial11)
import Tutorial2 (tutorial2)
import Tutorial3 (tutorial3)
import Tutorial4 (tutorial4)
import Tutorial5 (tutorial5)
import Tutorial6 (tutorial6)
import Tutorial7 (tutorial7)
import Tutorial8 (tutorial8)
import Tutorial9 (tutorial9)

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
        subRoute_ $ \case
          FrontendRoute_Tutorial1 -> tutorial1
          FrontendRoute_Tutorial2 -> tutorial2
          FrontendRoute_Tutorial3 -> tutorial3
          FrontendRoute_Tutorial4 -> tutorial4
          FrontendRoute_Tutorial5 -> tutorial5
          FrontendRoute_Tutorial6 -> tutorial6
          FrontendRoute_Tutorial7 -> tutorial7
          FrontendRoute_Tutorial8 -> tutorial8
          FrontendRoute_Tutorial9 -> tutorial9
          FrontendRoute_Tutorial10 -> tutorial10
          FrontendRoute_Tutorial11 -> tutorial11
          FrontendRoute_Home -> home
    }
