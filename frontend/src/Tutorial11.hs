{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tutorial11 (tutorial11) where

import Control.Monad.Fix
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Reflex.Dom.Core
import Tutorial7 (Op (..))
import Tutorial8 (numberPad)
import Tutorial9
  ( Button (..),
    CalcState (..),
    displayCalcState,
    initCalcState,
    updateCalcState,
  )
import Utils (button', buttonClasses)

tutorial11 ::
  forall t m.
  ( DomBuilder t m,
    MonadHold t m,
    MonadFix m,
    PostBuild t m
  ) =>
  m ()
tutorial11 = divClass "calculator" $ do
  rec el "div" $ dynText $ displayCalcState <$> calcState
      buttons <- elAttr
        "div"
        ("class" =: "grid grid-cols-4 grid-rows-5 gap-x-4 gap-y-4")
        $ do
          (numberButtons, bPeriod) <- elAttr
            "div"
            ("class" =: "row-start-2 col-span-3 row-span-4 grid grid-cols-3 gap-x-4 gap-y-4")
            $ do
              numberButtons <- numberPad
              bPeriod <- ("." <$) <$> button' "."
              pure (numberButtons, bPeriod)

          (opButtons, bEq) <- elAttr
            "div"
            ("class" =: "row-span-5 grid grid-cols-1 gap-x-4 gap-y-4")
            $ do
              bPlus <- opButton Plus "+" opState
              bMinus <- opButton Minus "-" opState
              bTimes <- opButton Times "*" opState
              bDivide <- opButton Divide "/" opState
              let opButtons = [bPlus, bMinus, bTimes, bDivide]
              bEq <- button' "="
              pure (opButtons, bEq)

          (bClear, bPlusMinus, bModulo) <- elAttr
            "div"
            ("class" =: "row-start-1 col-span-3 grid grid-cols-3 gap-x-4 gap-y-4")
            $ do
              bClear <- button' "C"
              bPlusMinus <- ("+/-" <$) <$> button' "+/-"
              bModulo <- opButton Modulo "%" opState
              pure (bClear, bPlusMinus, bModulo)

          let buttons' =
                leftmost
                  [ ButtonNumber <$> numberButtons,
                    ButtonNumber <$> bPeriod,
                    ButtonNumber <$> bPlusMinus,
                    ButtonOp <$> leftmost (bModulo : opButtons),
                    ButtonEq <$ bEq,
                    ButtonClear <$ bClear
                  ]
          pure buttons'
      calcState <- accumDyn updateCalcState initCalcState buttons
      let opState = _calcState_op <$> calcState
  pure ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <-
        elDynAttr'
          "button"
          (constDyn ("class" =: buttonClasses) <> (pickColor <$> selectedOp))
          $ text label
      pure (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
            then "style" =: "background: lightblue"
            else Map.empty
