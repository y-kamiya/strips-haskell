import Test.Hspec
import Algorithms.Strips
import Algorithms.Strips.Internal

-- example of block world
data Block = A | B deriving (Show, Eq, Ord, Enum)
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
main = hspec $ do
  describe "getConditionDiff" $ do
    it "returns diff and diff count of the two lists as tuple (both are empty list)" $ do
      getConditionDiff termsEmpty termsEmpty `shouldBe` (0, termsEmpty)
    it "returns diff and diff count of the two lists as tuple (both have same entry)" $ do
      getConditionDiff [HandEmpty] [HandEmpty] `shouldBe` (0, termsEmpty)
    it "returns diff and diff count of the two lists as tuple" $ do
      getConditionDiff [HandEmpty, IsTop A True, On A Table, On B (Object A)] [HandEmpty, IsTop A True, On B Table, On A (Object B)] `shouldBe` (2, [On A Table, On B (Object A)])

  describe "mergeNodes " $ do
    it "merges two nodeInfos of first and third argument except the element in second argument" $ do
      let n1 = buildTestNodeInfo 1 [HandEmpty]
          n2 = buildTestNodeInfo 2 [HandHas A]
          n3 = buildTestNodeInfo 3 [IsTop A True]
          n4 = buildTestNodeInfo 1 [HandHas A]
          n5 = buildTestNodeInfo 5 [On A Table]
      mergeNodes [n1, n2] [n3] [n3, n4, n5] `shouldBe` [n1, n4, n5]

  describe "getActionCandidates" $ do
    let extractActionTypes nodeInfo = map actionType $ getActionCandidates buildDomain nodeInfo
    it "gets []" $ do
      let n = buildTestNodeInfo 0 []
      extractActionTypes n `shouldBe` []
    it "gets [Pickup A]" $ do
      let n = buildTestNodeInfo 0 [HandHas A, IsTop A False]
      extractActionTypes n `shouldBe` [Pickup A]
    it "gets [Pickup A, Unstack A B]" $ do
      let n = buildTestNodeInfo 0 [HandHas A, IsTop A False, IsTop B True]
      extractActionTypes n `shouldBe` [Pickup A, Unstack A B]
    it "gets [Putdown A, Putdown B]" $ do
      let n = buildTestNodeInfo 0 [HandEmpty, IsTop A True, IsTop B True, On A Table, On B Table]
      extractActionTypes n `shouldBe` [Putdown A, Putdown B]

  describe "extractPlan" $ do
    let buildTestAction actionType = Action actionType termsEmpty termsEmpty 0
        buildNodeInfo action next = NodeInfo 0 0 0 termsEmpty termsEmpty action next
    it "gets empty plan" $ do
      extractPlan (buildNodeInfo NoAction NoNodeInfo) `shouldBe` ([] :: [BWActionType])
    it "gets plan" $ do
      let nodeInfo = buildNodeInfo (buildTestAction $ Pickup A)
                   $ buildNodeInfo (buildTestAction $ Pickup B)
                   $ buildNodeInfo (buildTestAction $ Putdown A)
                   $ buildNodeInfo NoAction NoNodeInfo
      extractPlan nodeInfo `shouldBe` [Pickup A, Pickup B, Putdown A]

  describe "strips" $ do
    it "gets correct plan" $ do
      let startCondition = [HandEmpty, IsTop A True, IsTop B False, On A (Object B), On B Table]
          goalCondition  = [HandEmpty, IsTop A False, IsTop B True, On B (Object A), On A Table]
          expected = [Unstack A B, Putdown A, Pickup B, Stack B A]
          nodeInfo = strips buildDomain startCondition goalCondition
      extractPlan nodeInfo `shouldBe` expected
      score nodeInfo `shouldBe` length expected

termsEmpty :: [BWTerm]
termsEmpty = []

buildTestNodeInfo :: Int -> [BWTerm] -> NodeInfo BWActionType BWTerm
buildTestNodeInfo score condition = NodeInfo 0 score 0 [] condition NoAction NoNodeInfo

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

