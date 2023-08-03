{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tutorial11 (tutorial11) where

import Control.Monad.Fix
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Reflex.Dom.Core
import Tutorial7 (Op (..))
import Tutorial8 (buttonClass, numberPad)
import Tutorial9
  ( Button (..),
    CalcState (..),
    displayCalcState,
    initCalcState,
    updateCalcState,
  )

tutorial11 :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial11 = divClass "calculator" $ do
  rec divClass "output" $ dynText $ displayCalcState <$> calcState
      buttons <- divClass "input" $ do
        (numberButtons, bPeriod) <- divClass "number-pad" $ do
          numberButtons <- numberPad
          bPeriod <- ("." <$) <$> buttonClass "number" "."
          return (numberButtons, bPeriod)
        (opButtons, bEq) <- divClass "ops-pad" $ do
          let opState = _calcState_op <$> calcState
          bPlus <- opButton Plus "+" opState
          bMinus <- opButton Minus "-" opState
          bTimes <- opButton Times "*" opState
          bDivide <- opButton Divide "/" opState
          let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
          bEq <- buttonClass "primary" "="
          return (opButtons, bEq)
        bClear <- divClass "other-pad" $ do
          bClear <- buttonClass "secondary" "C"
          _ <- buttonClass "secondary" "+/-"
          _ <- buttonClass "secondary" "%"
          return bClear
        let buttons' =
              leftmost
                [ ButtonNumber <$> numberButtons,
                  ButtonNumber <$> bPeriod,
                  ButtonOp <$> opButtons,
                  ButtonEq <$ bEq,
                  ButtonClear <$ bClear
                ]
        return buttons'
      calcState <- accumDyn updateCalcState initCalcState buttons
  return ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <-
        elDynAttr'
          "button"
          ( ("class" =: "primary" <>)
              <$> (pickColor <$> selectedOp)
          )
          $ text label
      return (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
            then "style" =: "background: lightblue"
            else Map.empty
