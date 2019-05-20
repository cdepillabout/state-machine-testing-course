{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (failing, firstOf, makeLenses, to, view, (.~), (^.), (+~))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Function ((&))
import           Data.Kind (Type)
import           Data.Maybe (isJust, isNothing)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType = Coffee | HotChocolate | Tea deriving (Bounded, Enum, Show, Eq)

-- This type and its associated fold might be useful.
data DrinkAdditive = Milk | Sugar deriving (Bounded, Enum, Show)

drinkAdditive :: a -> a -> DrinkAdditive -> a
drinkAdditive m _ Milk  = m
drinkAdditive _ s Sugar = s

data Model (v :: Type -> Type) = Model
  { _modelDrinkType :: DrinkType
  , _modelHasMug    :: Bool
  , _modelMilk      :: Int
  , _modelSugar     :: Int
  , _modelCoins     :: Int
  }
$(makeLenses ''Model)

newtype SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

data InsertCoins (v :: Type -> Type) = InsertCoins Int deriving Show

instance HTraversable InsertCoins where
  htraverse
    :: Applicative f
    => (forall a. g a -> f (h a)) -> InsertCoins g -> f (InsertCoins h)
  htraverse _ (InsertCoins int) = pure $ InsertCoins int

data RefundCoins (v :: Type -> Type) = RefundCoins deriving Show

instance HTraversable RefundCoins where
  htraverse _ _ = pure RefundCoins

newtype AddMilkSugar (v :: Type -> Type) = AddMilkSugar DrinkAdditive deriving Show

instance HTraversable AddMilkSugar where
  htraverse _ (AddMilkSugar a) = pure $ AddMilkSugar a

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkType mach = Command gen exec
  [ Update $ \m (SetDrinkType d) _execResult -> m
      & modelDrinkType .~ d
      & modelMilk      .~ 0
      & modelSugar     .~ 0

  , Ensure $ \_oldM newM _input drink -> case (_modelDrinkType newM, drink) of
      (Coffee, C.Coffee{})           -> success
      (HotChocolate, C.HotChocolate) -> success
      (Tea, C.Tea{})                 -> success
      _                              -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen _ = pure $ SetDrinkType <$> Gen.enumBounded

    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = do
      mach & case d of
        Coffee       -> C.coffee
        HotChocolate -> C.hotChocolate
        Tea          -> C.tea
      view C.drinkSetting <$> C.peek mach


cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug mach = Command gen exec
  [ Require $ \m _input -> m ^. modelHasMug
  , Update $ \m _input _execResult -> m & modelHasMug .~ False
  , Ensure $ \_oldM _newM _input -> assert . isNothing
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen m | _modelHasMug m = (pure . pure) TakeMug
          | otherwise = Nothing

    exec :: TakeMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      _ <- C.takeMug mach >>= evalEither
      view C.mug <$> C.peek mach

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec
  [ Require $ \m _input -> m ^. modelHasMug . to not
  , Update $ \m _input _execResult -> m & modelHasMug .~ True
  , Ensure $ \_oldM _newM _input -> assert . isJust
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen m | not $ _modelHasMug m = (pure . pure) AddMug
          | otherwise  = Nothing

    exec :: AddMug Concrete -> m (Maybe C.Mug)
    exec _ = do
      C.addMug mach >>= evalEither
      view C.mug <$> C.peek mach

cInsertCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cInsertCoins mach = Command gen exec
  [ Update update
  , Ensure ensure
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (InsertCoins Symbolic))
    gen _ = Just $ fmap InsertCoins (Gen.integral $ Range.constantFrom 1 1 10)

    exec :: InsertCoins Concrete -> m Int
    exec (InsertCoins int) = do
      C.insertCoins int mach
      fmap (view C.coins) (C.peek mach)

    update
      :: forall v
       . Ord1 v
      => Model v -> InsertCoins v -> Var Int v -> Model v
    update model (InsertCoins int) _ = model & modelCoins +~ int

    ensure
      :: Model Concrete
      -> Model Concrete
      -> InsertCoins Concrete
      -> Int
      -> Test ()
    ensure oldModel newModel (InsertCoins int) coinsInMachine = do
      let oldCoins = oldModel ^. modelCoins
          newCoins = newModel ^. modelCoins
      assert $ newCoins > oldCoins
      assert $ newCoins - oldCoins == int
      assert $ newCoins == coinsInMachine

cRefundCoins
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cRefundCoins mach = Command gen exec
  [ Update update
  , Ensure ensure
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (RefundCoins Symbolic))
    gen _ = Just $ pure RefundCoins

    exec
      :: RefundCoins Concrete
      -> m (Int, Int)  -- ^ (returned amount, current amount in machine)
    exec _ = do
      refundAmount <- C.refund mach
      amountInMachine <- fmap (view C.coins) (C.peek mach)
      pure (refundAmount, amountInMachine)

    update
      :: forall v
       . Ord1 v
      => Model v -> RefundCoins v -> Var (Int, Int) v -> Model v
    update model _ _ = model & modelCoins .~ 0

    ensure
      :: Model Concrete
      -> Model Concrete
      -> RefundCoins Concrete
      -> (Int, Int)
      -> Test ()
    ensure oldModel newModel _ (refundAmount, amountInMachine) = do
      let oldCoins = oldModel ^. modelCoins
          newCoins = newModel ^. modelCoins
      assert $ newCoins == 0
      assert $ newCoins == amountInMachine
      assert $ refundAmount == oldCoins

cAddMilkSugar
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMilkSugar mach = Command gen exec
  [ Require require
  , Update update
  , Ensure ensure
  ]
  where
    gen :: MonadGen g => Model Symbolic -> Maybe (g (AddMilkSugar Symbolic))
    gen model =
      case model ^. modelDrinkType of
        HotChocolate -> Nothing
        _ ->
          Just $ do
            milkOrSugar <- Gen.bool
            pure $ AddMilkSugar $ if milkOrSugar then Milk else Sugar

    exec
      :: AddMilkSugar Concrete
      -> m Int
    exec (AddMilkSugar additive) = do
      maybeAdditiveNum <-
        case additive of
          Milk -> do
            C.addMilk mach
            machState <- C.peek mach
            pure $
              firstOf
                (C.drinkSetting . (C._Coffee `failing` C._Tea) . C.milk)
                machState
          Sugar -> do
            C.addSugar mach
            machState <- C.peek mach
            pure $
              firstOf
                (C.drinkSetting . (C._Coffee `failing` C._Tea) . C.sugar)
                machState
      case maybeAdditiveNum of
        Nothing -> failure
        Just additiveNum -> pure additiveNum

    require :: Model Symbolic -> AddMilkSugar Symbolic -> Bool
    require model _ = (model ^. modelDrinkType) /= HotChocolate

    update
      :: forall v
       . Ord1 v
      => Model v -> AddMilkSugar v -> Var Int v -> Model v
    update model (AddMilkSugar Milk) _ = model & modelMilk +~ 1
    update model (AddMilkSugar Sugar) _ = model & modelSugar +~ 1

    ensure
      :: Model Concrete
      -> Model Concrete
      -> AddMilkSugar Concrete
      -> Int
      -> Test ()
    ensure oldModel newModel (AddMilkSugar Milk) milkAmount = do
      let oldMilkAmount = oldModel ^. modelMilk
          newMilkAmount = newModel ^. modelMilk
      assert $ oldMilkAmount + 1 == newMilkAmount
      assert $ newMilkAmount == milkAmount
    ensure oldModel newModel (AddMilkSugar Sugar) sugarAmount = do
      let oldSugarAmount = oldModel ^. modelSugar
          newSugarAmount = newModel ^. modelSugar
      assert $ oldSugarAmount + 1 == newSugarAmount
      assert $ newSugarAmount == sugarAmount

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False 0 0 0
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cAddMug
        , cTakeMug
        , cAddMilkSugar
        -- , cInsertCoins
        -- , cRefundCoins
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  liftIO $ print actions
  -- C.reset mach
  executeSequential initialModel actions
