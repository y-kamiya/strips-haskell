module Algorithms.Strips
( extractPlan
, strips
, ActionType
, Term
, Action(..)
, NodeInfo(..)
) where

import Control.Monad.Reader

import Algorithms.Strips.Internal

extractPlan :: (ActionType a, Term b) => NodeInfo a b -> [a]
extractPlan = extractPlan' []
  where
    extractPlan' :: (ActionType a, Term b) => [a] -> NodeInfo a b -> [a]
    extractPlan' plan NodeInfo { action = NoAction } = reverse plan
    extractPlan' plan NodeInfo { action = Action { actionType = actionType}, next = next } = extractPlan' (actionType:plan) next

strips :: (ActionType a, Term b) => [Action a b] -> [b] -> [b] -> NodeInfo a b
strips domain start goal = runReader search $ Env domain start goal
