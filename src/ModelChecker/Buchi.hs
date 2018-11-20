module ModelChecker.Buchi where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Alphabet letter = Set.Set letter

type TransitionFn state input = [(state, input, [state])]


data Automaton state letter = Automaton {
  states :: Set.Set state,
  alphabet :: Alphabet letter,
  transitions :: TransitionFn state letter,
  initial :: Set.Set state,
  acceptStates :: Set.Set state
} deriving (Show, Eq)
