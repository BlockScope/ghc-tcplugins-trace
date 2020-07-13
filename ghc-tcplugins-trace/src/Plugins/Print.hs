{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module Plugins.Print
    ( DebugPlugin(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , pprCtsStepProblem
    , pprCtsStepSolution
    , tracePlugin
    ) where

import Prelude hiding (showList)
import Data.Coerce (coerce)
import Language.Haskell.Printf (s)
import GHC.Corroborate (Ct, TcPluginM, TcPluginResult(..), tcPluginIO)

import Plugins.Print.Constraints (TraceCallCount(..), TraceCts(..), showList)

newtype TraceCarry = TraceCarry Bool
newtype TraceSolution = TraceSolution Bool

data DebugPlugin =
    DebugPlugin
        { traceCallCount :: TraceCallCount
        -- ^ Trace TcPlugin call count.
        , traceCts :: TraceCts
        -- ^ Trace GHC constraints.
        , traceCarry :: TraceCarry
        -- ^ Trace GHC constraints carried through conversion and solving.
        , traceSolution :: TraceSolution
        -- ^ Trace the solution, the @TcPluginResult@.
        }

pprCtsStepProblem
    :: DebugPlugin
    -> Maybe String
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStepProblem DebugPlugin{..} intro gCts dCts wCts = maybe [] return intro ++
    if not (coerce traceCts) then [] else
    [ [s|+++ GHC-Decs-Given = %s|] $ showList gCts
    , [s|+++ GHC-Decs-Derived = %s|] $ showList dCts
    , [s|+++ GHC-Decs-Wanted = %s|] $ showList wCts
    ]

pprCtsStepSolution :: DebugPlugin -> TcPluginResult -> [String]
pprCtsStepSolution DebugPlugin{..} x =
    if not (coerce traceSolution) then [] else
    case x of
        TcPluginContradiction cs ->
            [ [s|!!! SOLVE-Contradiction = %s|] $ showList cs ]

        TcPluginOk solved newCts ->
            [ [s|=== SOLVE-Solved = %s|] $ showList solved
            , [s|=== SOLVE-New-Wanted = %s|] $ showList newCts
            ]

tracePlugin :: DebugPlugin -> String -> TcPluginM ()
tracePlugin DebugPlugin{..} s'
    | coerce traceCallCount
        || coerce traceCts
        || coerce traceCarry
        || coerce traceSolution =
        if null s' then return () else tcPluginIO $ putStrLn s'

    | otherwise = return ()
