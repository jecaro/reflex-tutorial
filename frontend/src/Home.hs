{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Home (home) where

import Common.Route (FrontendRoute (..), routeLink')
import Control.Monad (forM_, unless)
import Data.Some (Some (..), withSome)
import Data.Universe (universe)
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute)
import Reflex.Dom.Core

home ::
  ( DomBuilder t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    Prerender t m
  ) =>
  m ()
home = do
  el "h1" $ text "Welcome"
  el "ul" $ do
    forM_ universe $ \route ->
      unless (route == Some FrontendRoute_Home) $ do
        el "li" $ withSome route routeLink'
