{-# LANGUAGE RecordWildCards #-}

module Plugins.Print
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , TracingFlags(..)
      -- * Pretty Printing
    , pprList
    , pprSolverCallCount
    , pprCtsStepProblem
    , pprCtsStepSolution
    , pprCts
    , tracePlugin
    ) where

import Data.Coerce (coerce)
import GHC.Corroborate (Ct, TcPluginM, TcPluginResult(..), tcPluginIO)

import Plugins.Print.Constraints (pprList, pprCts)
import Plugins.Print.Flags

tab :: String
tab = "    " 

-- | If tracing constraints, pretty print them.
pprCtsStepProblem
    :: TracingFlags
    -> Maybe String
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStepProblem TracingFlags{..} intro gCts dCts wCts = maybe [] return intro ++
    if not (coerce traceCts) then [] else pprCts gCts dCts wCts

-- | If tracing the solution, pretty print it.
pprCtsStepSolution :: TracingFlags -> TcPluginResult -> [String]
pprCtsStepSolution TracingFlags{..} x =
    if not (coerce traceSolution) then [] else
    case x of
        TcPluginContradiction cs ->
            [
                ( showString "  [solve]"
                . showString "\n"
                . showString tab
                . showString "contradiction = "
                . pprList 4 cs)
                ""
            ]

        TcPluginOk solved newCts ->
            [
                ( showString "  [solve]"
                . showString "\n"
                . showString tab
                . showString "solution = "
                . pprList 4 solved
                . showString "\n"
                . showString tab
                . showString "new-wanted = "
                . pprList 4 newCts)
                ""
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
