{-# LANGUAGE OverloadedStrings #-}

module Tutorial8 (numberPad, tutorial8) where

import Control.Monad.Fix
import Data.Text (Text, empty)
import Reflex.Dom.Core

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton n = buttonClass "number" n

tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = el "div" $ do
  numberButton <- numberPad
  clearButton <- button "C"
  let buttons =
        leftmost
          [ Nothing <$ clearButton,
            Just <$> numberButton
          ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  text " "
  dynText dstate
  where
    initialState :: Text
    initialState = empty

    collectButtonPresses :: Text -> Maybe Text -> Text
    collectButtonPresses state buttonPress =
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit
