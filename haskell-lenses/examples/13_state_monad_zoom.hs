{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module StateMonadOptics where

import Control.Lens
import Control.Monad.State

{- 
   Ref: Optics By Example - Chapter 13: Optics and Monads
   
   Lenses are extremely powerful when combined with the State Monad.
   
   Key Combinators:
   - use: View the focus of a lens in the current state (like 'get' but focused).
   - assign (.=): Set the focus of a lens in the current state.
   - modifying (%=): Modify the focus of a lens in the current state.
   - zoom: Execute a state action in a "smaller" state focused by a lens.
-}

data GameState = GameState
  { _score :: Int
  , _player :: Player
  } deriving Show

data Player = Player
  { _position :: (Int, Int)
  , _health :: Int
  } deriving Show

makeLenses ''GameState
makeLenses ''Player

-- Example 1: Simple state manipulation
updateScore :: State GameState ()
updateScore = do
  -- "use" gets the value
  currentScore <- use score
  -- "assign" sets the value (operator .=)
  score .= currentScore + 100
  -- "modifying" modifies it (operator %=)
  score %= (+50)

-- Example 2: Zooming
-- 'zoom' lets us run an action defined for 'Player' inside 'GameState'
playerLogic :: State Player ()
playerLogic = do
  health %= (\h -> max 0 (h - 10)) -- Take damage
  position . _1 += 1               -- Move right

gameTurn :: State GameState ()
gameTurn = do
  updateScore
  -- We can run playerLogic by zooming into the 'player' lens!
  zoom player playerLogic

exampleState :: GameState
exampleState = execState gameTurn (GameState 0 (Player (0, 0) 100))
-- Result: GameState {_score = 150, _player = Player {_position = (1,0), _health = 90}}
