{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Home (home) where

import Common.Route (FrontendRoute (..))
import Control.Monad (forM_, unless)
import Data.Some (Some (..), withSome)
import Data.Universe (universe)
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute)
import Reflex.Dom.Core
import Utils (routeLink')

home ::
  ( DomBuilder t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    Prerender t m
  ) =>
  m ()
home = do
  elAttr "div" ("class" =: "flex flex-col items-center space-y-4") $
    forM_ universe $ \route ->
      unless (route == Some FrontendRoute_Home) $
        withSome route routeLink'
