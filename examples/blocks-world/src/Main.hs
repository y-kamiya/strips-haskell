module Main where

import Algorithms.Strips

data Block = A | B | C deriving (Show, Eq, Ord, Enum)
data Object = Table | Object Block deriving (Eq, Ord, Show)

data BWTerm = HandEmpty
            | HandHas Block
            | IsTop Block Bool
            | On Block Object
            deriving (Eq, Ord, Show)

data BWActionType = Pickup Block
                  | Putdown Block
                  | Stack Block Block
                  | Unstack Block Block
                  deriving (Eq, Show)

instance ActionType BWActionType
instance Term BWTerm

main :: IO ()
main = do
  let startCondition = [HandEmpty, IsTop A True, IsTop B False, IsTop C True, On A (Object B), On B Table, On C Table]
  let goalCondition  = [HandEmpty, IsTop A False, IsTop B False, IsTop C True, On C (Object B), On B (Object A), On A Table]
  print "------------- domain ----------------"
  mapM_ print buildDomain
  print "------------- target ----------------"
  print $ "start: " ++ show startCondition
  print $ "goal: "  ++ show goalCondition
  print "------------- plan ----------------"
  let nodeInfo = strips buildDomain startCondition goalCondition
  mapM_ print $ extractPlan nodeInfo
  print $ "score: " ++ show (score nodeInfo)
  return ()

buildDomain :: [Action BWActionType BWTerm]
buildDomain = pickups ++ putdowns ++ stacks ++ unstacks
  where
    pickups  = map (buildAction 1 . Pickup) [A ..]
    putdowns = map (buildAction 1 . Putdown) [A ..]
    stacks   = map (buildAction 1 . uncurry Stack) perms
    unstacks = map (buildAction 1 . uncurry Unstack) perms
    perms = [(x, y) | x <- [A ..], y <- [A ..], x /= y]

buildAction :: Int -> BWActionType -> Action BWActionType BWTerm
buildAction cost aType@(Pickup x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = [HandEmpty, IsTop x True, On x Table]
        buildPost x = [HandHas x, IsTop x False]
buildAction cost aType@(Putdown x) = Action aType (buildPre x) (buildPost x) cost
  where buildPre x  = [HandHas x, IsTop x False]
        buildPost x = [HandEmpty, IsTop x True, On x Table]
buildAction cost aType@(Stack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandHas x, IsTop x False, IsTop y True]
        buildPost x y = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
buildAction cost aType@(Unstack x y) = Action aType (buildPre x y) (buildPost x y) cost
  where buildPre x y  = [HandEmpty, IsTop x True, IsTop y False, On x (Object y)]
        buildPost x y = [HandHas x, IsTop x False, IsTop y True]

