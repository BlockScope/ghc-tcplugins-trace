{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module Plugins.Print
    ( DebugPlugin(..)
    , TraceCarry(..)
    , pprCtsStep
    , tracePlugin
    ) where

import Prelude hiding (showList)
import Data.Coerce (coerce)
import Language.Haskell.Printf (s)
import GHC.Corroborate (Ct, TcPluginM, tcPluginIO)

import Plugins.Print.Constraints (TraceCallCount(..), TraceCts(..), showList)

newtype TraceCarry = TraceCarry Bool

data DebugPlugin =
    DebugPlugin
        { traceCallCount :: TraceCallCount
        -- ^ Trace TcPlugin call count
        , traceCts :: TraceCts
        -- ^ Trace GHC constraints
        , traceCarry :: TraceCarry
        -- ^ Trace GHC constraints carried through conversion and solving
        }

pprCtsStep
    :: DebugPlugin
    -> [Ct] -- ^ Given constraints
    -> [Ct] -- ^ Derived constraints
    -> [Ct] -- ^ Wanted constraints
    -> [String]
pprCtsStep DebugPlugin{..} gCts dCts wCts =
    if not (coerce traceCts) then [] else
    [ [s|+++ GHC-Decs-Given = %s|] $ showList gCts
    , [s|+++ GHC-Decs-Derived = %s|] $ showList dCts
    , [s|+++ GHC-Decs-Wanted = %s|] $ showList wCts
    ]

tracePlugin :: DebugPlugin -> String -> TcPluginM ()
tracePlugin DebugPlugin{..} s'
    | coerce traceCallCount || coerce traceCts || coerce traceCarry =
        if null s' then return () else tcPluginIO $ putStrLn s'

    | otherwise = return ()
