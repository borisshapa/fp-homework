{-# LANGUAGE InstanceSigs #-}

module Task8.Comonad19
  ( -- * The @Task8.Comonad19@ functions
    defaultParameters
  , mkGrid
  , oneInfected
  , step
  , stepN
  ) where

import Control.Comonad (Comonad (..))
import System.Random (StdGen, mkStdGen, randomR)

import Task8.ListZipper
import Task8.Grid

-- | Human health status.
data HealthState
  = Susceptible     -- Healthy, at risk
  | Incubation Int  -- Infected. The virus will be in an
                    -- incubation state for n days

  | Symptoms Int    -- Will be sick for n days
  | Recovered Int   -- Healthy, will have immunity for n days

instance Show HealthState where
  show  :: HealthState -> String
  show Susceptible = "_"
  show (Incubation _) = "*"
  show (Symptoms _) = "#"
  show (Recovered _) = "@"

-- | Human model in Covid-19 simulation
data Person = Person
  { -- Health status
    pHealthState :: HealthState

    -- | A special random number generator 
    -- to calculate the probability
    -- of getting infected
  , pRandGen :: StdGen
  }

instance Show Person where
  show :: Person -> String
  show (Person state _) = show state

-- | Parameters of Covid-19 simulation
data Parameters = Parameters
  { -- | Chance of getting infected without immunity.
    pInfectionLikelihood :: Double
    -- | The duration of the incubation period of the virus
  , pIncubationPerion :: Int
    -- | The duration of the illness
  , pIllnessPeriod :: Int
    -- | The duration of the immunity to virus
  , pImmunityPeriod :: Int
  } deriving (Show)

-- | Default parameters of Covid-19 simulations
defaultParameters :: Parameters
defaultParameters = Parameters 0.1 2 10 30

-- | Creates ListZipper with healhy people
mkRow ::  ListZipper Person
mkRow = LZ ls x rs
  where
    x = Person Susceptible (mkStdGen 0)
    ls = map (Person Susceptible . mkStdGen) [1, 2 ..]
    rs = map (Person Susceptible . mkStdGen) [1, 3 ..]

-- | Creates Grid with healthy people
mkGrid :: Grid Person
mkGrid = Grid $ duplicate mkRow

-- | Creates Grid with sick person (in focus)
oneInfected :: Parameters -> Grid Person
oneInfected Parameters{pIncubationPerion = period} =
    gridWrite person{ pHealthState = Incubation period} g
  where
    g = mkGrid
    person = gridRead g

-- | Count of sick neighbours
illNeighbours :: Grid Person -> Int
illNeighbours g = length $ illN
  where
    allN = map (\direction -> gridRead $ direction g) neighbours
    illN = filter (\(Person state _) ->
                    case state of
                      (Incubation _) -> True
                      (Symptoms _) -> True
                      _ -> False) allN

-- | Person changes his healthy state depending on his probability
-- to get infected and count of sick people around him.
probIll :: Parameters -> Grid Person -> Person
probIll (Parameters likelihood incP _ _) g =
  if (p > threshold)
    then Person (Incubation incP) newRand
    else Person Susceptible newRand
  where
    p = likelihood * (fromIntegral $ illNeighbours g) / 4
    (threshold, newRand) = randomR (0, 1) (pRandGen $ gridRead g)

-- | What happens to people every day
rule :: Parameters -> Grid Person -> Person
rule p@(Parameters _ _ illP immP) g = newState
  where
    Person state rnd = extract g
    newState = case state of
      Susceptible -> probIll p g

      Incubation 1 -> Person (Symptoms illP) rnd
      Incubation n -> Person (Incubation (n - 1)) rnd

      Symptoms 1 -> Person (Recovered immP) rnd
      Symptoms n -> Person (Symptoms (n - 1)) rnd

      Recovered 1 -> Person Susceptible rnd
      Recovered n -> Person (Recovered (n - 1)) rnd

-- | One step simulation (simulates one day)
step :: Parameters -> Grid Person -> Grid Person
step p = extend (rule p)

-- | N step simulation (simulates N days)
stepN :: Parameters -> Int -> Grid Person -> Grid Person
stepN _ 0 g = g
stepN p n g = stepN p (n - 1) (step p g)