{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route
  ( BackendRoute (..),
    FrontendRoute (..),
    fullRouteEncoder,
    routeLink',
    title,
    url,
  )
where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route
  ( Encoder,
    FullRoute (..),
    PageName,
    R,
    SegmentResult (..),
    mkFullRouteEncoder,
    unitEncoder,
    pattern (:/),
  )
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Obelisk.Route.TH (deriveRouteComponent)
import Reflex.Dom.Core

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()

-- You can define any routes that will be handled specially by the backend here.
-- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Tutorial1 :: FrontendRoute ()
  FrontendRoute_Tutorial2 :: FrontendRoute ()
  FrontendRoute_Tutorial3 :: FrontendRoute ()
  FrontendRoute_Tutorial4 :: FrontendRoute ()
  FrontendRoute_Tutorial5 :: FrontendRoute ()
  FrontendRoute_Tutorial6 :: FrontendRoute ()
  FrontendRoute_Tutorial7 :: FrontendRoute ()
  FrontendRoute_Tutorial8 :: FrontendRoute ()
  FrontendRoute_Tutorial9 :: FrontendRoute ()
  FrontendRoute_Tutorial10 :: FrontendRoute ()
  FrontendRoute_Tutorial11 :: FrontendRoute ()

-- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder ::
  Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend BackendRoute_Missing :/ ())
    ( \case
        BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    )
    ( \case
        FrontendRoute_Home -> PathEnd $ unitEncoder mempty
        FrontendRoute_Tutorial1 -> pathSegment FrontendRoute_Tutorial1
        FrontendRoute_Tutorial2 -> pathSegment FrontendRoute_Tutorial2
        FrontendRoute_Tutorial3 -> pathSegment FrontendRoute_Tutorial3
        FrontendRoute_Tutorial4 -> pathSegment FrontendRoute_Tutorial4
        FrontendRoute_Tutorial5 -> pathSegment FrontendRoute_Tutorial5
        FrontendRoute_Tutorial6 -> pathSegment FrontendRoute_Tutorial6
        FrontendRoute_Tutorial7 -> pathSegment FrontendRoute_Tutorial7
        FrontendRoute_Tutorial8 -> pathSegment FrontendRoute_Tutorial8
        FrontendRoute_Tutorial9 -> pathSegment FrontendRoute_Tutorial9
        FrontendRoute_Tutorial10 -> pathSegment FrontendRoute_Tutorial10
        FrontendRoute_Tutorial11 -> pathSegment FrontendRoute_Tutorial11
    )
  where
    pathSegment route = PathSegment (segmentText route) $ unitEncoder mempty
    segmentText route = T.map spaceToDash . T.toLower $ title route
    spaceToDash ' ' = '-'
    spaceToDash c = c

title :: FrontendRoute a -> Text
title FrontendRoute_Home = "Home"
title FrontendRoute_Tutorial1 = "Tutorial 1"
title FrontendRoute_Tutorial2 = "Tutorial 2"
title FrontendRoute_Tutorial3 = "Tutorial 3"
title FrontendRoute_Tutorial4 = "Tutorial 4"
title FrontendRoute_Tutorial5 = "Tutorial 5"
title FrontendRoute_Tutorial6 = "Tutorial 6"
title FrontendRoute_Tutorial7 = "Tutorial 7"
title FrontendRoute_Tutorial8 = "Tutorial 8"
title FrontendRoute_Tutorial9 = "Tutorial 9"
title FrontendRoute_Tutorial10 = "Tutorial 10"
title FrontendRoute_Tutorial11 = "Tutorial 11"

url :: FrontendRoute a -> R FrontendRoute
url route =
  route :/ case route of
    FrontendRoute_Home -> ()
    FrontendRoute_Tutorial1 -> ()
    FrontendRoute_Tutorial2 -> ()
    FrontendRoute_Tutorial3 -> ()
    FrontendRoute_Tutorial4 -> ()
    FrontendRoute_Tutorial5 -> ()
    FrontendRoute_Tutorial6 -> ()
    FrontendRoute_Tutorial7 -> ()
    FrontendRoute_Tutorial8 -> ()
    FrontendRoute_Tutorial9 -> ()
    FrontendRoute_Tutorial10 -> ()
    FrontendRoute_Tutorial11 -> ()

routeLink' ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  FrontendRoute a ->
  m ()
routeLink' route = routeLink (url route) . text $ title route

concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
