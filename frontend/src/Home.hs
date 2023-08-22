{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Home (home) where

import Common.Route (FrontendRoute (..), title, url)
import Control.Monad (forM_, unless)
import Data.Some (Some (..))
import Data.Universe (universe)
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
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
  forM_ universe $ \route ->
    unless (route == Some FrontendRoute_Home) $
      el "h4" . routeLink (url route) . text $ title route
