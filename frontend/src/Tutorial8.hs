{-# LANGUAGE OverloadedStrings #-}

module Tutorial8 (numberPad, tutorial8) where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text, empty)
import Reflex.Dom.Core
import Utils (button', oneColumnClasses)

numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> button' "7"
  b8 <- ("8" <$) <$> button' "8"
  b9 <- ("9" <$) <$> button' "9"
  b4 <- ("4" <$) <$> button' "4"
  b5 <- ("5" <$) <$> button' "5"
  b6 <- ("6" <$) <$> button' "6"
  b1 <- ("1" <$) <$> button' "1"
  b2 <- ("2" <$) <$> button' "2"
  b3 <- ("3" <$) <$> button' "3"
  b0 <- ("0" <$) <$> button' "0"
  pure $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]

tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = elAttr "div" ("class" =: oneColumnClasses) $ do
  numberButton <- numberPad
  clearButton <- button' "C"
  let buttons =
        leftmost
          [ Nothing <$ clearButton,
            Just <$> numberButton
          ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  el "div" $ dynText dstate
  where
    initialState :: Text
    initialState = empty

    collectButtonPresses :: Text -> Maybe Text -> Text
    collectButtonPresses state buttonPress =
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit
