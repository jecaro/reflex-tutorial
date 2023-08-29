{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module LocalStorage (localStorage) where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
  ( FromJSVal,
    JSM,
    ToJSVal,
    fromJSValUnchecked,
    js,
    js1,
    js2,
    jsg,
    liftJSM,
    maybeNullOrUndefined',
  )
import Reflex.Dom.Core

default (Text)

location :: Text
location = "some-location"

save :: ToJSVal a => Text -> a -> JSM ()
save key val =
  void $
    jsg "window"
      ^. js "localStorage"
      ^. js2 "setItem" key val

load :: FromJSVal a => Text -> JSM (Maybe a)
load key =
  maybeNullOrUndefined' fromJSValUnchecked
    =<< jsg "window"
    ^. js "localStorage"
    ^. js1 "getItem" key

clear :: Text -> JSM ()
clear key =
  void $
    jsg "window"
      ^. js "localStorage"
      ^. js1 "removeItem" key

localStorage ::
  ( DomBuilder t m,
    Prerender t0 m,
    PerformEvent t (Client m),
    Prerender t m,
    PostBuild t m,
    MonadHold t m
  ) =>
  m ()
localStorage =
  el "div" $ do
    -- create an input element and return a behavior
    inputText <- current . value <$> inputElement def
    saveButton <- button "save"

    prerender_ (pure ())
      . performEvent_
      -- save the current value of the input
      . fmap (liftJSM . save location)
      -- convert the click event to an event that contains the current value of
      -- the input
      $ tag inputText saveButton

    loadButton <- button "load"
    loadedValue :: Event t (Maybe Text) <-
      -- fire the javascript event when the load button is clicked by converting
      -- that Dynamic Event to the final Event
      fmap switchDyn
        . prerender (pure never)
        -- returns a dynamic attached to the button event and which value is
        -- the event of the javascript call
        . performEvent
        -- load the value from localStorage
        . fmap (liftJSM . const (load location))
        $ loadButton

    clearButton <- button "clear"
    prerender_ (pure ())
      . performEvent_
      . fmap (liftJSM . const (clear location))
      $ clearButton

    dynText =<< fmap (T.pack . show) <$> holdDyn Nothing loadedValue
