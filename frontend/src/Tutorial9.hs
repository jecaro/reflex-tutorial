{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Tutorial9
  ( Button (..),
    CalcState (..),
    displayCalcState,
    initCalcState,
    tutorial9,
    updateCalcState,
  )
where

import Control.Monad.Fix
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Text.Read
import Tutorial7 (Op (..), runOp)
import Tutorial8 (numberPad)

data CalcState = CalcState
  { _calcState_acc :: Double, -- accumulator
    _calcState_op :: Maybe Op, -- most recently requested operation
    _calcState_input :: Text -- current input
  }
  deriving stock (Show)

data Button
  = ButtonNumber Text
  | ButtonOp Op
  | ButtonEq
  | ButtonClear

initCalcState :: CalcState
initCalcState = CalcState 0 Nothing ""

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input) btn =
  case btn of
    ButtonNumber d ->
      if d == "." && isJust (T.find (== '.') input)
        then state
        else CalcState acc mOp (input <> d)
    ButtonOp pushedOp -> applyOp state (Just pushedOp)
    ButtonEq -> applyOp state Nothing
    ButtonClear -> initCalcState

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input) mOp' =
  if T.null input
    then CalcState acc mOp' input
    else case readMaybe (T.unpack input) of
      Nothing -> state -- this should be unreachable
      Just x -> case mOp of
        Nothing -> CalcState x mOp' ""
        Just op -> CalcState (runOp op acc x) mOp' ""

displayCalcState :: CalcState -> Text
displayCalcState (CalcState acc _op input) =
  if T.null input
    then T.pack (show acc)
    else input

tutorial9 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial9 = el "div" $ do
  numberButtons <- numberPad
  bPeriod <- ("." <$) <$> button "."
  bPlus <- (Plus <$) <$> button "+"
  bMinus <- (Minus <$) <$> button "-"
  bTimes <- (Times <$) <$> button "*"
  bDivide <- (Divide <$) <$> button "/"
  let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
  bEq <- button "="
  bClear <- button "C"
  let buttons =
        leftmost
          [ ButtonNumber <$> numberButtons,
            ButtonNumber <$> bPeriod,
            ButtonOp <$> opButtons,
            ButtonEq <$ bEq,
            ButtonClear <$ bClear
          ]
  calcState <- accumDyn updateCalcState initCalcState buttons
  text " "
  dynText (displayCalcState <$> calcState)
