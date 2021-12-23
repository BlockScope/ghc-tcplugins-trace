{-# LANGUAGE QuasiQuotes #-}

module Plugins.Print.Flags
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , TracingFlags(..)
      -- * Pretty Printing
    , pprSolverCallCount 
    ) where

import Language.Haskell.Printf (s)

-- | Flag for controlling tracing counts of each time the plugin is called.
newtype TraceCallCount = TraceCallCount Bool

-- | Pretty print the call count if tracing them.
pprSolverCallCount :: TraceCallCount -> Int -> String
pprSolverCallCount (TraceCallCount callCount) n
    | callCount = [s|>>> GHC-TcPlugin #%d|] n
    | otherwise = ""

-- | Flag for controlling tracing of type checking constraints.
newtype TraceCts = TraceCts Bool

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