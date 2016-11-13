module Algorithms.Strips.Internal where

import Data.List ((\\), null, union, sort, sortBy)
import Data.Function (on)
import qualified Data.Map as M
import Control.Monad.Reader

class (Eq a, Show a ) => ActionType a

class (Eq b, Ord b, Show b) => Term b

data Action a b = NoAction
                | Action {
                    actionType    :: a -- ^ type of action
                  , preCondition  :: [b] -- ^ list of conditions that must be satisfied before the action is executed
                  , postCondition :: [b] -- ^ list of conditions that must be satisfied after the action is executed
                  , actionCost    :: Int -- ^ required cost to execute action
                  } deriving (Eq, Show)

data NodeInfo a b = NoNodeInfo
                  | NodeInfo {
                      realCost  :: Int -- ^ total cost actually required to reach the current node
                    , score     :: Int -- ^ the sum of actual cost and estimate cost
                    , diffCount :: Int -- ^ count of condition difference between goal and current node (= estimate cost)
                    , diff      :: [b] -- ^ condition difference between goal and current node
                    , condition :: [b] -- ^ current condition
                    , action    :: Action a b -- ^ action to go to next node
                    , next      :: NodeInfo a b -- ^ next node
                    } deriving (Eq, Show)

data Env a b = Env { envDomain :: [Action a b]
                   , envStart  :: [b]
                   , envGoal   :: [b]
                   }

search :: (ActionType a, Term b) => Reader (Env a b) (NodeInfo a b)
search = buildGoalNodeInfo >>= (\goalNodeInfo -> searchNext [goalNodeInfo] [])
  where
    buildGoalNodeInfo :: (ActionType a, Term b) => Reader (Env a b) (NodeInfo a b)
    buildGoalNodeInfo = do
      start <- asks envStart
      goal <- asks envGoal
      let (estimateCost, conditionDiff) = getConditionDiff start goal
      return $ NodeInfo 0 estimateCost estimateCost conditionDiff goal NoAction NoNodeInfo

searchNext :: (ActionType a, Term b) => [NodeInfo a b] -> [NodeInfo a b] -> Reader (Env a b) (NodeInfo a b)
searchNext [] _ = return NoNodeInfo
searchNext openList@(nodeInfo:rest) closeList
  | diffCount nodeInfo == 0 = return nodeInfo
  | otherwise = buildOpenList openList closeList >>= flip searchNext (nodeInfo:closeList)

buildOpenList :: (ActionType a, Term b) => [NodeInfo a b] -> [NodeInfo a b] -> Reader (Env a b) [NodeInfo a b]
buildOpenList (nodeInfo:rest) closeList = return . sortBy (compare `on` score) . mergeNodes rest closeList =<< getNextNodes nodeInfo

getNextNodes :: (ActionType a, Term b) => NodeInfo a b -> Reader (Env a b) [NodeInfo a b]
getNextNodes nodeInfo = do
  domain <- asks envDomain
  mapM (buildNodeInfo nodeInfo) $ getActionCandidates domain nodeInfo
  where
    buildNodeInfo :: (ActionType a, Term b) => NodeInfo a b -> Action a b -> Reader (Env a b) (NodeInfo a b)
    buildNodeInfo nodeInfo action = do
      start <- asks envStart
      let (eCost, diff) = getConditionDiff newCondition start
          newCondition = snd (getConditionDiff (condition nodeInfo) (postCondition action)) `union` preCondition action
          rCost = realCost nodeInfo + actionCost action
          score = rCost + eCost
      return $ NodeInfo rCost score eCost diff newCondition action nodeInfo

getConditionDiff :: (Term b) => [b] -> [b] -> (Int, [b])
getConditionDiff dest src = let diff = dest \\ src in (length diff, diff)

mergeNodes :: (ActionType a, Term b) => [NodeInfo a b] -> [NodeInfo a b] -> [NodeInfo a b] -> [NodeInfo a b]
mergeNodes openList closeList newNodes = M.elems $ M.unionWith replaceByCondition openMap $ newNodeMap M.\\ closeMap
  where openMap    = M.fromList $ map toTuple openList
        closeMap   = M.fromList $ map toTuple closeList
        newNodeMap = M.fromList $ map toTuple newNodes
        toTuple nodeInfo = (sort $ condition nodeInfo, nodeInfo)
        replaceByCondition old new = if score old <= score new then old else new

getActionCandidates :: (ActionType a, Term b) => [Action a b] -> NodeInfo a b -> [Action a b]
getActionCandidates domain nodeInfo = filter include domain
  where include action = null $ postCondition action \\ condition nodeInfo





