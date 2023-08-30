{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tutorial10 (tutorial10) where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Tutorial7 (Op (..))
import Tutorial8 (numberPad)
import Tutorial9 (Button (..), CalcState (..), initCalcState, updateCalcState)
import Utils (button', buttonClasses, oneColumnClasses)

tutorial10 :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial10 = elAttr "div" ("class" =: oneColumnClasses) $ do
  rec numberButtons <- numberPad
      bPeriod <- ("." <$) <$> button' "."
      let opState = _calcState_op <$> calcState
      bPlus <- opButton Plus "+" opState
      bMinus <- opButton Minus "-" opState
      bTimes <- opButton Times "*" opState
      bDivide <- opButton Divide "/" opState
      let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
      bEq <- button' "="
      bClear <- button' "C"
      let buttons =
            leftmost
              [ ButtonNumber <$> numberButtons,
                ButtonNumber <$> bPeriod,
                ButtonOp <$> opButtons,
                ButtonEq <$ bEq,
                ButtonClear <$ bClear
              ]
      calcState <- accumDyn updateCalcState initCalcState buttons
      el "div" $ dynText (T.pack . show . _calcState_acc <$> calcState)
      el "div" $ dynText (_calcState_input <$> calcState)
  pure ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <-
        elDynAttr'
          "button"
          ((pickColor <$> selectedOp) <> constDyn ("class" =: buttonClasses))
          $ text label
      pure (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
            then "style" =: "color: red"
            else Map.empty
