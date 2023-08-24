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
  el "div" $ do
    rec boardEvents <- el "table" $ do
          line1 <- el "tr" $ do
            ev11 <- el "td" $ dynButton 0 dynState
            ev12 <- el "td" $ dynButton 1 dynState
            ev13 <- el "td" $ dynButton 2 dynState
            pure [ev11, ev12, ev13]
          line2 <- el "tr" $ do
            ev21 <- el "td" $ dynButton 3 dynState
            ev22 <- el "td" $ dynButton 4 dynState
            ev23 <- el "td" $ dynButton 5 dynState
            pure [ev21, ev22, ev23]
          line3 <- el "tr" $ do
            ev31 <- el "td" $ dynButton 6 dynState
            ev32 <- el "td" $ dynButton 7 dynState
            ev33 <- el "td" $ dynButton 8 dynState
            pure [ev31, ev32, ev33]
          pure $ concat [line1, line2, line3]
        dynText $ message <$> dynState
        el "br" blank
        resetEvent <- (Reset <$) <$> button "Reset"
        dynState <- foldDyn next init $ leftmost (resetEvent : boardEvents)
    pure ()

dynButton ::
  (DomBuilder t m, PostBuild t m) =>
  Int ->
  Dynamic t [SquareState] ->
  m (Event t Button)
dynButton i dynState = do
  let dynButtonState = fromMaybe NotPlayed . preview (ix i) <$> dynState
  (e, _) <- el' "button" $ dynText (label <$> dynButtonState)
  pure . (Square i <$) $ domEvent Click e
