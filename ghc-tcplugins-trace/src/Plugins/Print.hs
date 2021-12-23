{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module Plugins.Print
    ( -- * Trace Control Flags
      TracingFlags(..)
    , TraceCarry(..)
    , TraceSolution(..)
      -- * Pretty Printing
    , pprCtsStepProblem
    , pprCtsStepSolution
    , tracePlugin
    ) where

import Data.Coerce (coerce)
import Language.Haskell.Printf (s)
import GHC.Corroborate (Ct, TcPluginM, TcPluginResult(..), tcPluginIO)

import Plugins.Print.Constraints (TraceCallCount(..), TraceCts(..), pprList)

-- | Flag for controlling tracing of the carry.
newtype TraceCarry = TraceCarry Bool

-- | Flag for controlling tracing of the solution of type checking.
newtype TraceSolution = TraceSolution Bool

-- | A group of flags for tracing.
data TracingFlags =
    TracingFlags
        { traceCallCount :: TraceCallCount
        -- ^ Trace TcPlugin call count.
        , traceCts :: TraceCts
        -- ^ Trace GHC constraints.
        , traceCarry :: TraceCarry
        -- ^ Trace GHC constraints carried through conversion and solving.
        , traceSolution :: TraceSolution
        -- ^ Trace the solution, the @TcPluginResult@.
        }

-- | If tracing constraints, pretty print them.
pprCtsStepProblem
    :: TracingFlags
    -> Maybe String
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStepProblem TracingFlags{..} intro gCts dCts wCts = maybe [] return intro ++
    if not (coerce traceCts) then [] else
    [ [s|+++ GHC-Decs-Given = %s|] $ pprList gCts
    , [s|+++ GHC-Decs-Derived = %s|] $ pprList dCts
    , [s|+++ GHC-Decs-Wanted = %s|] $ pprList wCts
    ]

-- | If tracing the solution, pretty print it.
pprCtsStepSolution :: TracingFlags -> TcPluginResult -> [String]
pprCtsStepSolution TracingFlags{..} x =
    if not (coerce traceSolution) then [] else
    case x of
        TcPluginContradiction cs ->
            [ [s|!!! SOLVE-Contradiction = %s|] $ pprList cs ]

        TcPluginOk solved newCts ->
            [ [s|=== SOLVE-Solved = %s|] $ pprList solved
            , [s|=== SOLVE-New-Wanted = %s|] $ pprList newCts
            ]

-- | Trace the given string if any of the tracing flags are switched on.
tracePlugin :: TracingFlags -> String -> TcPluginM ()
tracePlugin TracingFlags{..} s'
    | coerce traceCallCount
        || coerce traceCts
        || coerce traceCarry
        || coerce traceSolution =
        if null s' then return () else tcPluginIO $ putStrLn s'

    | otherwise = return ()
