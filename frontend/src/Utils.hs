{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( button',
    buttonClasses,
    inputClasses,
    oneColumnClasses,
    routeLink',
  )
where

import Common.Route (FrontendRoute (..), title, url)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route (R)
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLinkAttr)
import Reflex.Dom.Core

routeLink' ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  FrontendRoute a ->
  m ()
routeLink' route =
  routeLinkAttr
    ("class" =: buttonClasses)
    (url route)
    . text
    $ title route

button' :: DomBuilder t m => Text -> m (Event t ())
button' label = do
  (e, _) <- elAttr' "button" ("class" =: buttonClasses) $ text label
  pure $ domEvent Click e

oneColumnClasses :: Text
oneColumnClasses =
  T.unwords
    [ "flex",
      "flex-col",
      "space-y-4"
    ]

buttonClasses :: Text
buttonClasses =
  T.unwords
    [ "bg-blue-500",
      "hover:bg-blue-600",
      "text-white",
      "px-4",
      "py-2",
      "rounded",
      "text-center"
    ]

inputClasses :: Text
inputClasses =
  T.unwords
    [ "bg-gray-50",
      "border",
      "border-gray-300",
      "rounded-lg",
      "focus:ring-blue-500",
      "focus:border-blue-500",
      "p-2.5"
    ]
