{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine as C
import           Control.Lens (view)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Kind (Type)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

data DrinkType
  = Coffee
  | HotChocolate
  | Tea
  deriving (Enum, Bounded, Eq, Show)

data HasMug = NoMug | HasMug

data Model (v :: Type -> Type) = Model DrinkType HasMug

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

-- Replace these with one SetDrinkType data type whose constructor
-- takes the type of drink to select as an argument. Don't forget to
-- add a HTraversable instance.

-- data SetDrinkCoffee (v :: Type -> Type) = SetDrinkCoffee deriving Show
-- data SetDrinkHotChocolate (v :: Type -> Type) = SetDrinkHotChocolate deriving Show
-- data SetDrinkTea (v :: Type -> Type) = SetDrinkTea deriving Show

data SetDrink (v :: Type -> Type) = SetDrink DrinkType deriving Show

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

-- instance HTraversable SetDrinkCoffee where
--   htraverse _ _ = pure SetDrinkCoffee

-- instance HTraversable SetDrinkHotChocolate where
--   htraverse _ _ = pure SetDrinkHotChocolate

-- instance HTraversable SetDrinkTea where
--   htraverse _ _ = pure SetDrinkTea

instance HTraversable SetDrink where
  htraverse
    :: Applicative f
    => (forall a. g a -> f (h a)) -> SetDrink g -> f (SetDrink h)
  htraverse _ (SetDrink drink) = pure $ SetDrink drink

-- cSetDrinkCoffee
--   :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
--   => C.Machine
--   -> Command g m Model
-- cSetDrinkCoffee mach = Command gen exec
--   [ Update $ \(Model _ hasMug) _ _ -> Model Coffee hasMug
--   , Ensure $ \_ _ _ drink -> case drink of
--       C.Coffee{} -> success
--       _ -> failure
--   ]
--   where
--     gen :: Model Symbolic -> Maybe (g (SetDrinkCoffee Symbolic))
--     gen _ = Just $ pure SetDrinkCoffee

--     exec :: SetDrinkCoffee Concrete -> m C.Drink
--     exec _ = do
--       C.coffee mach
--       view C.drinkSetting <$> C.peek mach

-- cSetDrinkHotChocolate
--   :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
--   => C.Machine
--   -> Command g m Model
-- cSetDrinkHotChocolate mach = Command gen exec
--   [ Update $ \(Model _ hasMug) _ _ -> Model HotChocolate hasMug
--   , Ensure $ \_ _ _ drink -> case drink of
--       C.HotChocolate -> success
--       _ -> failure
--   ]
--   where
--     gen :: Model Symbolic -> Maybe (g (SetDrinkHotChocolate Symbolic))
--     gen _ = Just $ pure SetDrinkHotChocolate

--     exec :: SetDrinkHotChocolate Concrete -> m C.Drink
--     exec _ = do
--       C.hotChocolate mach
--       view C.drinkSetting <$> C.peek mach

-- cSetDrinkTea
--   :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
--   => C.Machine
--   -> Command g m Model
-- cSetDrinkTea mach = Command gen exec
--   [ Update $ \(Model _ hasMug) _ _ -> Model Tea hasMug
--   , Ensure $ \_ _ _ drink -> case drink of
--       C.Tea{} -> success
--       _ -> failure
--   ]
--   where
--     gen :: Model Symbolic -> Maybe (g (SetDrinkTea Symbolic))
--     gen _ = Just $ pure SetDrinkTea

--     exec :: SetDrinkTea Concrete -> m C.Drink
--     exec _ = do
--       C.tea mach
--       view C.drinkSetting <$> C.peek mach

-- Replace the three command definitions above with one cSetDrinkType
-- command definition below, which will the SetDrink data type you
-- defined above.

cSetDrink
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrink mach = Command gen exec callbacks
  where
    gen :: Model Symbolic -> Maybe (g (SetDrink Symbolic))
    gen _ = Just $ fmap SetDrink Gen.enumBounded

    exec :: SetDrink Concrete  -> m C.Drink
    exec (SetDrink drink) = do
      case drink of
        Coffee -> C.coffee mach
        HotChocolate -> C.hotChocolate mach
        Tea -> C.tea mach
      view C.drinkSetting <$> C.peek mach

    callbacks :: [Callback SetDrink C.Drink Model]
    callbacks =
      [ Update updateCurrDrink
      , Ensure currDrinkEnsure
      ]

    updateCurrDrink
      :: forall v
       . Ord1 v
      => Model v -> SetDrink v -> Var C.Drink v -> Model v
    updateCurrDrink (Model _ hasMug) (SetDrink drink) _ = Model drink hasMug

    currDrinkEnsure ::
         Model Concrete
      -> Model Concrete
      -> SetDrink Concrete
      -> C.Drink
      -> Test ()
    currDrinkEnsure _ _ (SetDrink Coffee) C.Coffee{} = success
    currDrinkEnsure _ _ (SetDrink HotChocolate) C.HotChocolate{} = success
    currDrinkEnsure _ _ (SetDrink Tea) C.Tea{} = success
    currDrinkEnsure _ _ _ _ = failure

-- Fill in these command definitions to take and replace the mug in
-- the machine. Use the TakeMug and AddMug types you defined
-- previously.
--
-- You should only return a generator when it makes sense: taking a
-- mug when it's in the machine, or adding a mug when the machine is
-- empty. You will also need `Require` callbacks that enforce this.
--
-- You should also write an `Ensure` callback that verifies that your
-- action worked.

cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug mach = Command gen exec callbacks
  where
    gen :: Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen (Model _ HasMug) = Just $ pure TakeMug
    gen (Model _ NoMug) = Nothing

    exec :: TakeMug Concrete -> m (Either C.MachineError C.Mug)
    exec TakeMug = C.takeMug mach

    callbacks :: [Callback TakeMug (Either C.MachineError C.Mug) Model]
    callbacks =
      [ Update update
      , Ensure ensure
      , Require require
      ]

    update
      :: forall v
       . Ord1 v
      => Model v
      -> TakeMug v
      -- Do I need to be looking at this?
      --
      -- I don't need to be looking at this right now.
      -> Var (Either C.MachineError C.Mug) v
      -> Model v
    update (Model drink _) _ _ = Model drink NoMug
    -- I don't actually want to have to deal with this case?
    -- update (Model drink NoMug) TakeMug _ = Model drink NoMug

    -- Is this ensuring that the output model is correct?  Or the actual output
    -- value is correct?  Or both?
    --
    -- Kinda both.  It depends on whether you want to assume your update function
    -- is correct.
    ensure ::
         Model Concrete
      -> Model Concrete
      -> TakeMug Concrete
      -> Either C.MachineError C.Mug
      -> Test ()
    ensure _ (Model _ NoMug) _ (Right _) = success
    ensure _ _ _ _ = failure

    require :: Model Symbolic -> TakeMug Symbolic -> Bool
    require (Model _ HasMug) _ = True
    require _ _ = False

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec callbacks
  where
    gen :: Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen (Model _ HasMug) = Nothing
    gen (Model _ NoMug) = Just $ pure AddMug

    exec :: AddMug Concrete -> m ()
    exec AddMug = do
      res <- C.addMug mach
      evalEither res

    callbacks :: [Callback AddMug () Model]
    callbacks =
      [ Update update
      , Ensure ensure
      , Require require
      ]

    update
      :: forall v
       . Ord1 v
      => Model v
      -> AddMug v
      -> Var () v
      -> Model v
    update (Model drink _) _ _ = Model drink HasMug

    ensure ::
         Model Concrete
      -> Model Concrete
      -> AddMug Concrete
      -> ()
      -> Test ()
    ensure (Model _ NoMug) (Model _ HasMug) _ () = success
    ensure _ _ _ _ = failure

    require :: Model Symbolic -> AddMug Symbolic -> Bool
    require (Model _ NoMug) _ = True
    require _ _ = False

stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate NoMug
      commands = ($ mach) <$>
        [ cSetDrink
        , cTakeMug
        , cAddMug
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  -- liftIO $ print actions
  -- C.reset mach
  executeSequential initialModel actions
