{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module LocalStorage (localStorage) where

import Data.Text (Text)
import qualified Data.Text as T
import JSDOM (currentWindowUnchecked)
import JSDOM.Generated.Storage (getItem, removeItem, setItem)
import JSDOM.Generated.Window (getLocalStorage)
import JSDOM.Types (FromJSString, Storage, ToJSString)
import Language.Javascript.JSaddle (JSM, liftJSM)
import Reflex.Dom.Core
import Utils (button', inputClasses, oneColumnClasses)

default (Text)

location :: Text
location = "some-location"

getLocalStorage' :: JSM Storage
getLocalStorage' = currentWindowUnchecked >>= getLocalStorage

save :: ToJSString a => Text -> a -> JSM ()
save key val = getLocalStorage' >>= \ls -> setItem ls key val

load :: FromJSString a => Text -> JSM (Maybe a)
load key = getLocalStorage' >>= flip getItem key

clear :: Text -> JSM ()
clear key = getLocalStorage' >>= flip removeItem key

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
  elAttr "div" ("class" =: oneColumnClasses) $ do
    -- create an input element and return a behavior
    inputText <-
      current . value
        <$> inputElement (def & initialAttributes .~ ("class" =: inputClasses))
    saveButton <- button' "save"

    prerender_ (pure ())
      . performEvent_
      -- save the current value of the input
      . fmap (liftJSM . save location)
      -- convert the click event to an event that contains the current value of
      -- the input
      $ tag inputText saveButton

    loadButton <- button' "load"
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

    clearButton <- button' "clear"
    prerender_ (pure ())
      . performEvent_
      . fmap (liftJSM . const (clear location))
      $ clearButton

    el "div" $ dynText . fmap (T.pack . show) =<< holdDyn Nothing loadedValue
