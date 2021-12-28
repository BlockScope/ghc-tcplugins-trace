{-# LANGUAGE RecordWildCards #-}

module Plugins.Print
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , TracingFlags(..)
      -- * Pretty Printing
    , Indent(..)
    , pprList
    , pprSolverCallCount
    , pprCtsStepProblem
    , pprCtsStepSolution
    , pprCts
    , tracePlugin
    ) where

import Data.Coerce (coerce)
import GHC.Corroborate (Ct, TcPluginM, TcPluginResult(..), tcPluginIO)

import Plugins.Print.Constraints (Indent(..), pprList, pprCts)
import Plugins.Print.Flags

-- | If tracing constraints, pretty print them.
pprCtsStepProblem
    :: Indent
    -> TracingFlags
    -> Maybe String
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStepProblem indent TracingFlags{..} intro gCts dCts wCts = maybe [] return intro ++
    if not (coerce traceCts) then [] else pprCts indent gCts dCts wCts

-- | If tracing the solution, pretty print it.
pprCtsStepSolution :: Indent -> TracingFlags -> TcPluginResult -> [String]
pprCtsStepSolution indent@(Indent i) TracingFlags{..} x =
    if not (coerce traceSolution) then [] else
    case x of
        TcPluginContradiction cs ->
            [
                ( tab
                . showString "[solve]"
                . showString "\n"
                . tabtab
                . showString "contradiction = "
                . pprList j cs)
                ""
            ]

        TcPluginOk solved newCts ->
            [
                ( tab
                . showString "[solve]"
                . showString "\n"
                . tabtab
                . showString "solution = "
                . pprList j solved
                . showString "\n"
                . tabtab
                . showString "new-wanted = "
                . pprList j newCts)
                ""
            ]
    where
        tab = showString $ replicate (2 * i) ' '
        tabtab = showString $ replicate (2 * (i + 1)) ' '
        j = indent + 1

-- | Trace the given string if any of the tracing flags are switched on.
tracePlugin :: TracingFlags -> String -> TcPluginM ()
tracePlugin TracingFlags{..} s'
    | coerce traceCallCount
        || coerce traceCts
        || coerce traceCarry
        || coerce traceSolution =
        if null s' then return () else tcPluginIO $ putStrLn s'

    | otherwise = return ()
