{-# LANGUAGE RecordWildCards #-}

module Plugins.Print
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceSolution(..)
    , DebugCts(..)
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
    :: DebugCts
    -> String
    -> Indent
    -> Maybe String
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStepProblem DebugCts{..} section indent intro gCts dCts wCts =
    if not (coerce traceCts)
        then []
        else maybe [] return intro ++ pprCts section indent gCts dCts wCts

-- | If tracing the solution, pretty print it.
pprCtsStepSolution :: DebugCts -> String -> Indent -> TcPluginResult -> [String]
pprCtsStepSolution DebugCts{..} title iIndent@(Indent i) x =
    if not (coerce traceSolution) then [] else
    case x of
        TcPluginContradiction cs ->
            [
                ( tab
                . showChar '['
                . showString title
                . showChar ']'
                . showChar '\n'
                . tabtab
                . showString "contradiction = "
                . pprList jIndent cs)
                ""
            ]

        TcPluginOk solved newCts ->
            [
                ( tab
                . showChar '['
                . showString title
                . showChar ']'
                . showChar '\n'
                . tabtab
                . showString "solution = "
                . pprList jIndent solved
                . showChar '\n'
                . tabtab
                . showString "new-wanted = "
                . pprList jIndent newCts)
                ""
            ]
    where
        tab = showString $ replicate (2 * i) ' '
        tabtab = showString $ replicate (2 * (i + 1)) ' '
        jIndent = iIndent + 1

-- | Trace the given string if any of the tracing flags are switched on.
tracePlugin :: DebugCts -> String -> TcPluginM ()
tracePlugin DebugCts{..} s'
    | coerce traceCallCount
        || coerce traceCts
        || coerce traceSolution =
        if null s' then return () else tcPluginIO $ putStrLn s'

    | otherwise = return ()
