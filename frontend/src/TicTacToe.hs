{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}

module TicTacToe (ticTacToe) where

import Control.Lens (ix, preview, (%~))
import Control.Monad.Fix (MonadFix)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Reset)
import Utils (button', buttonClasses, oneColumnClasses)
import Prelude hiding (init, lines)

data Player = X | O
  deriving stock (Eq, Show)

data SquareState = Played Player | NotPlayed
  deriving stock (Eq, Show)

data Button = Square Int | Reset
  deriving stock (Eq, Show)

data Status = Next Player | GameOver (Maybe Player)
  deriving stock (Eq, Show)

label :: SquareState -> T.Text
label (Played X) = "X"
label (Played O) = "O"
label NotPlayed = "_"

init :: [SquareState]
init = replicate 9 NotPlayed

next :: Button -> [SquareState] -> [SquareState]
next Reset _ = init
next (Square i) state =
  case status state of
    GameOver _ -> state
    Next player -> state & ix i %~ ifNotAlreadyPlayed (Played player)
  where
    ifNotAlreadyPlayed new NotPlayed = new
    ifNotAlreadyPlayed _ old = old

status :: [SquareState] -> Status
status state
  | player@(Just _) <- winner state = GameOver player
  | NotPlayed `elem` state = Next nextPlayer
  | otherwise = GameOver Nothing
  where
    nextPlayer
      | length xs > length os = O
      | otherwise = X
    (xs, os) = partition (== Played X) $ filter (/= NotPlayed) state

winner :: [SquareState] -> Maybe Player
winner state
  | any (all (== Played X)) lines = Just X
  | any (all (== Played O)) lines = Just O
  | otherwise = Nothing
  where
    toPlayer = fmap (fromMaybe NotPlayed . (`preview` state) . ix)
    lines =
      fmap
        toPlayer
        [ [0, 1, 2],
          [3, 4, 5],
          [6, 7, 8],
          [0, 3, 6],
          [1, 4, 7],
          [2, 5, 8],
          [0, 4, 8],
          [2, 4, 6]
        ]

message :: [SquareState] -> T.Text
message state = case status state of
  Next player -> "Next player: " <> T.pack (show player)
  GameOver (Just player) -> "Winner: " <> T.pack (show player)
  GameOver Nothing -> "Draw"

ticTacToe :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m) => m ()
ticTacToe =
  elAttr "div" ("class" =: oneColumnClasses) $ do
    rec boardEvents <- elAttr
          "div"
          ("class" =: "grid grid-cols-3 gap-x-4 gap-y-4")
          $ do
            ev11 <- dynButton 0 dynState
            ev12 <- dynButton 1 dynState
            ev13 <- dynButton 2 dynState
            ev21 <- dynButton 3 dynState
            ev22 <- dynButton 4 dynState
            ev23 <- dynButton 5 dynState
            ev31 <- dynButton 6 dynState
            ev32 <- dynButton 7 dynState
            ev33 <- dynButton 8 dynState
            pure $ [ev11, ev12, ev13, ev21, ev22, ev23, ev31, ev32, ev33]
        resetEvent <- (Reset <$) <$> button' "Reset"
        el "div" . dynText $ message <$> dynState
        dynState <- foldDyn next init $ leftmost (resetEvent : boardEvents)
    pure ()

dynButton ::
  (DomBuilder t m, PostBuild t m) =>
  Int ->
  Dynamic t [SquareState] ->
  m (Event t Button)
dynButton i dynState = do
  let dynButtonState = fromMaybe NotPlayed . preview (ix i) <$> dynState
  (e, _) <-
    elAttr' "button" ("class" =: buttonClasses) $
      dynText (label <$> dynButtonState)
  pure . (Square i <$) $ domEvent Click e
