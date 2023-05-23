{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}    -- i added
{-# LANGUAGE LambdaCase #-}             -- i added
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

-- | Benchmark (exbudget and script size) for Plutus scripts
module Benchmark (
  -- | * Types
  Benchmark,
  NamedBenchmark (..),
  ScriptSizeBytes,
  -- | * Benchmark an arbitraty Plutus script
  benchmarkScript,
  benchmarkScript',
  -- | * Benchmark entrypoints
  bench,
  bench',
  benchGroup,
  benchMain,
  -- | * Working with benchmark results
  decodeBenchmarks,
  diffBenchmarks,
  renderDiffTable,
  renderBudgetTable,
  renderAdjBudgetTable,
) where

import Control.Arrow ((&&&))
import Control.Monad.Writer
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Coerce (coerce)
import Data.Either.Combinators (mapLeft)
import Data.Map.Strict qualified as Map
import Data.Vector (
  Vector,
  (!),
 )
import Data.Vector qualified as Vector
import System.Environment (getArgs)
import Text.PrettyPrint.Boxes ((//))
import Text.PrettyPrint.Boxes qualified as B

import Control.Monad.Except (runExcept)
import Data.ByteString.Short qualified as ByteString.Short
import Data.Csv (
  DefaultOrdered,
  ToField,
  ToNamedRecord,
  header,
  namedRecord,
  (.!),
  (.=),
 )
import Data.Csv qualified as Csv
import Data.Int (Int64)
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
-- import Plutarch (
--   ClosedTerm,
--   Config (..),
--   TracingMode (..),
--   compile,
--   printTerm,
--  )
-- import Plutarch.Script (
--   Script,
--   unScript,
--  )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (
  defaultCostModelParams,
 )
import PlutusLedgerApi.V2 ( 
--import Plutus.V2.Ledger.Api  (
-- import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
  ProtocolVersion (..),
  SerialisedScript,
  VerboseMode (Verbose),
  evaluateScriptCounting,
  mkEvaluationContext,
  serialiseUPLC,
 )

vasilPV :: ProtocolVersion
vasilPV = ProtocolVersion 7 0

-- | Benchmark the given script
benchmarkScript :: String -> Script -> NamedBenchmark
benchmarkScript name = NamedBenchmark . (name,) . benchmarkScript'

benchmarkScript' :: Script -> Benchmark
benchmarkScript' =
  uncurry mkBenchmark . (evalScriptCounting &&& (fromInteger . toInteger . ByteString.Short.length)) . serialiseScriptShort
  where
    mkBenchmark :: ExBudget -> Int64 -> Benchmark
    mkBenchmark (ExBudget cpu mem) = Benchmark cpu mem . ScriptSizeBytes

    serialiseScriptShort :: Script -> ByteString.Short.ShortByteString
    serialiseScriptShort = serialiseUPLC . unScript
    evalScriptCounting :: HasCallStack => SerialisedScript -> ExBudget
    evalScriptCounting script = either error id $ do
      let costModel = fromJust defaultCostModelParams
      evalContext <- mapLeft show $ runExcept $ runWriterT $ mkEvaluationContext (Map.elems costModel)
      let (_logout, e) = evaluateScriptCounting vasilPV Verbose (fst evalContext) script []
      mapLeft show e

-- in  case e of
--       Left  evalErr  -> error ("Eval Error: " <> show evalErr)
--       Right exbudget -> exbudget

data Benchmark = Benchmark
  { exBudgetCPU :: ExCPU
  -- ^ CPU budget used by the script
  , exBudgetMemory :: ExMemory
  -- ^ Memory budget used by the script
  , scriptSizeBytes :: ScriptSizeBytes
  -- ^ Size of Plutus script in bytes
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype ScriptSizeBytes = ScriptSizeBytes Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToField)
  deriving newtype (ToJSON)

{- | A `Benchmark` with a name.

 Handy for writing CSV files with headers.
-}
newtype NamedBenchmark = NamedBenchmark (String, Benchmark)
  deriving stock (Show, Generic)
  deriving newtype (ToJSON)

instance ToNamedRecord NamedBenchmark where
  toNamedRecord (NamedBenchmark (name, Benchmark {..})) =
    namedRecord ["name" .= name, "cpu" .= exBudgetCPU, "mem" .= exBudgetMemory, "size" .= scriptSizeBytes]

instance DefaultOrdered NamedBenchmark where
  headerOrder _ = header ["name", "cpu", "mem", "size"]

-- | Create a benchmark group with a shared prefix
benchGroup :: String -> [[NamedBenchmark]] -> [NamedBenchmark]
benchGroup groupName bs =
  [NamedBenchmark (groupName ++ ":" ++ name, benchmark) | NamedBenchmark (name, benchmark) <- concat bs]

benchmarkConfig :: Config
benchmarkConfig = Config {tracingMode = NoTracing}

-- | Create a benchmark with a name
bench :: String -> ClosedTerm a -> [NamedBenchmark]
bench name prog = either (error . T.unpack) id $ do
  script <- compile benchmarkConfig prog
  return [coerce $ benchmarkScript name script]

-- | Create a benchmark with itself as name
bench' :: ClosedTerm a -> [NamedBenchmark]
bench' prog = bench (init . drop (length ("(program 1.0.0 " :: String)) $ printTerm benchmarkConfig prog) prog

-- | Decode benchmark results from a CSV file
decodeBenchmarks :: ByteString.Lazy.ByteString -> Either String [NamedBenchmark]
decodeBenchmarks =
  let (#!) :: Num a => Vector Csv.Field -> Int -> Csv.Parser a
      (#!) v f = fmap fromInteger . Csv.parseField $ v ! f
   in fmap Vector.toList
        <$> Csv.decodeWithP
          ( \case
              v
                | length v == 4 ->
                    fmap NamedBenchmark $ (,) <$> v .! 0 <*> (Benchmark <$> v #! 1 <*> v #! 2 <*> v #! 3)
              _ | otherwise -> mzero
          )
          Csv.defaultDecodeOptions
          Csv.HasHeader

data BenchmarkDiffs = BenchmarkDiffs
  { dropped :: [NamedBenchmark]
  , changed :: [BenchmarkDiff]
  , added :: [NamedBenchmark]
  }
  deriving stock (Show, Generic)

data BenchmarkDiff = BenchmarkDiff
  { benchmark :: Benchmark
  , change :: (Double, Double, Double)
  , name :: String
  }
  deriving stock (Show, Generic)

diffBenchmark :: String -> Benchmark -> Benchmark -> Maybe BenchmarkDiff
diffBenchmark name (Benchmark (ExCPU oldCpu) (ExMemory oldMem) (ScriptSizeBytes oldSize)) new@(Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes size))
  | oldCpu /= cpu || oldMem /= mem || oldSize /= size =
      let pctChange old new =
            softRound (fromInteger (toInteger new - toInteger old) / fromInteger (toInteger $ max old new) * 100)

          softRound n = fromInteger @Double (round @Double @Integer n * 10) / 10
       in Just $
            BenchmarkDiff
              { benchmark = new
              , change = (pctChange oldCpu cpu, pctChange oldMem mem, pctChange oldSize size)
              , name = name
              }
  | otherwise =
      Nothing

diffBenchmarks :: [NamedBenchmark] -> [NamedBenchmark] -> BenchmarkDiffs
diffBenchmarks (Map.fromList . coerce -> old) (Map.fromList . coerce -> new) =
  BenchmarkDiffs
    { changed = Map.elems $ Map.mapMaybeWithKey (\k new -> old Map.!? k >>= \old -> diffBenchmark k old new) new
    , dropped = coerce . Map.toList $ old `Map.difference` new
    , added = coerce . Map.toList $ new `Map.difference` old
    }

renderDiffTable :: BenchmarkDiffs -> B.Box
renderDiffTable (BenchmarkDiffs dropped changed added) =
  let renderChange change
        | abs change <= 0.01 = B.text ""
        | otherwise = B.text $ if change > 0 then "+" <> show change <> "%" else show change <> "%"

      renderResult old diff tag = [B.text $ show old <> "(" <> tag <> ")", renderChange diff]

      renderBenchmarkDiff :: BenchmarkDiff -> [B.Box]
      renderBenchmarkDiff (BenchmarkDiff (Benchmark (ExCPU x) (ExMemory y) (ScriptSizeBytes z)) (dx, dy, dz) name) =
        mconcat [[B.text name], renderResult x dx "cpu", renderResult y dy "mem", renderResult z dz "bytes"]
   in B.vsep
        1
        B.top
        [ if null dropped then B.nullBox else B.text "Dropped benchmarks:" // renderBudgetTable dropped
        , if null changed
            then B.nullBox
            else B.text "Changed benchmarks:" // renderTable [renderBenchmarkDiff change | change <- changed]
        , if null added then B.nullBox else B.text "Added benchmarks:" // renderBudgetTable added
        ]

renderTable :: [[B.Box]] -> B.Box
renderTable rows =
  let alignments =
        -- Align all but the first column to the right, because they represent numeric values.
        B.left : repeat B.right
   in B.hsep 2 B.left . fmap (uncurry B.vcat) $ zip alignments (List.transpose rows)

renderBudgetTable :: [NamedBenchmark] -> B.Box
renderBudgetTable bs =
  renderTable $
    [ [B.text name, B.text $ show cpu <> "(cpu)", B.text $ show mem <> "(mem)", B.text $ show sz <> "(bytes)"]
    | NamedBenchmark (name, Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes sz)) <- bs
    ]

renderAdjBudgetTable :: [NamedBenchmark] -> B.Box
renderAdjBudgetTable bs =
  renderTable $
    [B.text "Name", B.text "Script Size (kb)", B.text "CPU", B.text "Memory", B.text "Script Size (bytes)"]
      : [ [ B.text name
          , B.text $ show (fromIntegral sz / 1000 :: Double) <> "(kb)"
          , B.text $ show cpu <> "(cpu)"
          , B.text $ show mem <> "(mem)"
          , B.text $ show sz <> "(bytes)"
          ]
        | NamedBenchmark (name, Benchmark (ExCPU cpu) (ExMemory mem) (ScriptSizeBytes sz)) <- bs
        ]

benchMain :: [NamedBenchmark] -> IO ()
benchMain benchmarks =
  getArgs >>= \case
    ["--csv"] -> ByteString.Lazy.putStr $ Csv.encodeDefaultOrderedByName benchmarks
    _ -> do
      let csv = Csv.encodeDefaultOrderedByName benchmarks
      ByteString.Lazy.writeFile "bench.csv" csv
      putStrLn "Wrote to bench.csv:"
      putStrLn . B.render $ renderBudgetTable benchmarks
