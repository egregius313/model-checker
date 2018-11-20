module ModelChecker.TransitionSystem where

import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data Action a = Action a | InternalAction deriving (Eq, Show)
type TransitionFn state action = Map.Map (state, Action action) (Set.Set state)


-- Make actions orderable. Makes working with sets and maps easier.
instance Ord a => Ord (Action a) where
  compare InternalAction InternalAction = EQ
  compare InternalAction _ = LT
  compare _ InternalAction = GT
  compare (Action a1) (Action a2) = compare a1 a2


data TransitionSystem state action proposition = TransitionSystem {
  states :: Set.Set state,
  actions :: Set.Set (Action action),
  transitions :: TransitionFn state action,
  initial :: Set.Set state,
  propositions :: Set.Set proposition,
  labeling :: Map.Map state (Set.Set proposition)
  } deriving (Eq, Show)


makeActions :: Ord a => [a] -> Set.Set (Action a)
makeActions as =
  Set.fromList $ InternalAction : map Action as


predecessors :: Eq state => TransitionSystem state action proposition -> state -> [state]
predecessors ts state =
  [src | ((src, _), dsts) <- Map.assocs $ transitions ts, state `elem` dsts]


successors :: (Ord state, Ord action) => TransitionSystem state action proposition -> state -> [state]
successors ts state =
  let next = Set.map (\action -> transitions ts !? (state, action)) $ actions ts
  in Set.toList $ foldl Set.union Set.empty [successor | Just successor <- Set.toList next]


terminals :: (Ord s, Ord a) => TransitionSystem s a p -> Set.Set s
terminals ts = Set.filter (null . successors ts) $ states ts

makeTransitions :: (Ord state, Ord a) => [(state, Action a, state)] -> TransitionFn state a
makeTransitions = let addElement m k v =
                        case m !? k of
                          Nothing -> Map.insert k (Set.singleton v) m
                          _ -> Map.adjust (Set.insert v) k m in
                    foldl (\m (from, when, to) -> addElement m (from, when) to) Map.empty 


isReachable :: Ord state => TransitionSystem state action proposition -> state -> Bool
isReachable ts state
  | Set.member state $ initial ts = True
  | preds == [] = False
  | otherwise = any (isReachable ts) preds
  where preds = predecessors ts state


transition :: (Ord state, Ord action) => TransitionSystem state action proposition -> Set.Set state -> Set.Set state
transition ts ss =
  Set.foldl Set.union Set.empty $ Set.map (Set.fromList . successors ts) ss

after :: (Ord action, Ord state) => TransitionSystem state action proposition -> Int -> Set.Set state
after ts n = (iterate (transition ts) $ initial ts) !! n


satisfies :: Eq proposition => TransitionSystem state action proposition -> proposition -> Set.Set state -> Bool
satisfies ts proposition states =
  undefined


makeTransition :: state -> action -> state -> (state, Action action, state)
makeTransition from when to = (from, Action when, to)

internalTransition :: state -> state -> (state, Action action, state) 
internalTransition from to = (from, InternalAction, to)

-- Drink example

data DrinkAction = InsertCoin | GetSoda | GetBeer deriving (Eq, Ord, Show)
data DrinkState = Pay | Select | Beer | Soda deriving (Eq, Ord, Show)
data DrinkProps = Paid | Drink deriving (Eq, Ord, Show)

ts1 :: TransitionSystem DrinkState DrinkAction DrinkProps
ts1 = TransitionSystem {
  states = Set.fromList [Pay, Select, Beer, Soda],
  actions = makeActions [InsertCoin, GetSoda, GetBeer],
  transitions = makeTransitions [makeTransition Pay InsertCoin Select,
                                 internalTransition Select Beer,
                                 internalTransition Select Soda,
                                 makeTransition Beer GetBeer Pay,
                                 makeTransition Soda GetSoda Pay
                                ],
  initial = Set.singleton Pay,
  propositions = Set.fromList [Paid, Drink],
  labeling = Map.fromAscList [
      (Pay, Set.empty),
      (Select, Set.singleton Paid),
      (Beer, Set.fromList [Paid, Drink]),
      (Soda, Set.fromList [Paid, Drink])
      ]
  }


-- 

data State2 = S0 | S1 | S2 | S3 deriving (Eq, Ord, Show)
data Letter = A | B | C deriving (Eq, Ord, Show)

ts2 :: TransitionSystem State2 Letter Char
ts2 = TransitionSystem {
  states = Set.fromList [S0, S1, S2, S3],
  actions = makeActions [A, B, C],
  transitions = makeTransitions [makeTransition S0 A S1,
                                 makeTransition S1 B S2,
                                 makeTransition S2 C S2,
                                 makeTransition S2 B S3
                                ],
  initial = Set.singleton S0,
  propositions = Set.empty,
  labeling = Map.empty
  }
