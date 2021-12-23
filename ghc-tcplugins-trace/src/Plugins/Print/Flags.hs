module Plugins.Print.Flags
    ( -- * Trace Control Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , TracingFlags(..)
    ) where

-- | Flag for controlling tracing counts of each time the plugin is called.
newtype TraceCallCount = TraceCallCount Bool

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
