{-# LANGUAGE RecordWildCards #-}

module Plugins.Print
    ( DebugPlugin(..)
    , TraceCarry(..)
    , tracePlugin
    ) where

import Data.Coerce (coerce)
import GHC.Corroborate (TcPluginM, tcPluginIO)

import Plugins.Print.Constraints (TraceCallCount(..), TraceCts(..))

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

tracePlugin :: DebugPlugin -> String -> TcPluginM ()
tracePlugin DebugPlugin{..} s'
    | coerce traceCallCount || coerce traceCts || coerce traceCarry =
            tcPluginIO $ putStrLn s'

    | otherwise = return ()
