{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Common.Route
import Control.Monad.Fix
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core
import Text.Read

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Obelisk Minimal Example"
        elAttr
          "script"
          ( "type" =: "application/javascript"
              <> "src" =: $(static "lib.js")
          )
          blank
        elAttr
          "link"
          ( "href" =: $(static "main.css")
              <> "type" =: "text/css"
              <> "rel" =: "stylesheet"
          )
          blank,
      _frontend_body = do
        el "h1" $ text "Welcome to Obelisk!"

        tutorial1
        el "hr" blank
        tutorial2
        el "hr" blank
        tutorial3
        el "hr" blank
        tutorial4
        el "hr" blank
        tutorial5
        el "hr" blank
        tutorial6
        el "hr" blank
        tutorial7
    }

tutorial1 :: DomBuilder t m => m ()
tutorial1 = el "div" $ text "Welcome to Reflex"

tutorial2 :: DomBuilder t m => m ()
tutorial2 = el "div" $ do
  el "p" $ text "Reflex is:"
  el "ul" $ do
    el "li" $ text "Efficient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"

tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  text " "
  dynText $ _inputElement_value t

tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = el "div" $ do
  t <-
    inputElement $
      def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes
          .~ ("type" =: "number")
  text " "
  dynText $ _inputElement_value t

tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  text " "
  dynText numberString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
              .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n

tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = el "div" $ do
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = "
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (pack . show) result
  dynText resultString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
              .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n

data Op = Plus | Minus | Times | Divide
  deriving stock (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp Plus = (+)
runOp Minus = (-)
runOp Times = (*)
runOp Divide = (/)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  op <- _dropdown_value <$> dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) op values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
              .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
