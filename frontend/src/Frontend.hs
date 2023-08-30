{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Common.Route (FrontendRoute (..))
import Data.Dependent.Sum (DSum (..))
import Home (home)
import Javascript (javascript)
import LocalStorage (localStorage)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (RouteToUrl, RoutedT, SetRoute, askRoute, subRoute_)
import Reflex.Dom.Core
import TicTacToe (ticTacToe)
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
import Utils (routeLink')

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Reflex playground"
        elAttr
          "meta"
          ( "name" =: "viewport"
              <> "contents" =: "width=device-width, initial-scale=1.0"
          )
          blank
        elAttr
          "script"
          ( "type" =: "application/javascript"
              <> "src" =: $(static "js/lib.js")
          )
          blank
        elAttr
          "link"
          ( "href" =: $(static "css/styles.css")
              <> "type" =: "text/css"
              <> "rel" =: "stylesheet"
          )
          blank,
      _frontend_body = do
        elAttr "div" ("class" =: "relative mt-4 mb-4 mr-4 ml-4") $
          subRoute_ $ \case
            FrontendRoute_Home -> home
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
            FrontendRoute_TicTacToe -> ticTacToe
            FrontendRoute_LocalStorage -> localStorage
            FrontendRoute_Javascript -> javascript
        elAttr "div" ("class" =: "absolute bottom-8 right-8") $
          homeLink
    }

homeLink ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m
  ) =>
  RoutedT t (R FrontendRoute) m ()
homeLink = dyn_ . fmap (unlessOnHome routeLinkHome) =<< askRoute
  where
    unlessOnHome :: DomBuilder t m => m () -> R FrontendRoute -> m ()
    unlessOnHome _ (FrontendRoute_Home :=> _) = blank
    unlessOnHome a _ = a
    routeLinkHome = routeLink' FrontendRoute_Home
