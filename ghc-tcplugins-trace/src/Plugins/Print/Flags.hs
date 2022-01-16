module Plugins.Print.Flags
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceCarry(..)
    , TraceSolution(..)
    , DebugCts(..)
      -- * Pretty Printing
    , pprSolverCallCount 
    ) where

import Plugins.Print.Constraints (Indent(..))

-- | Flag for controlling tracing counts of each time the plugin is called.
newtype TraceCallCount = TraceCallCount Bool

-- | Pretty print the calls of this plugin as a count if tracing calls.
pprSolverCallCount :: Indent -> TraceCallCount -> Int -> String
pprSolverCallCount (Indent i) (TraceCallCount callCount) n
    | callCount = let tab = showString $ replicate (2 * i) ' ' in
      ( showString "[ghc-tcplugin]"
      . showChar '\n'
      . tab
      . showString "call = "
      . shows n)
      ""
    | otherwise = ""

-- | Flag for controlling tracing of type checking constraints.
newtype TraceCts = TraceCts Bool

-- | Flag for controlling tracing of the carry.
newtype TraceCarry = TraceCarry Bool

-- | Flag for controlling tracing of the solution of type checking.
newtype TraceSolution = TraceSolution Bool

-- | A group of flags for tracing constraits.
data DebugCts =
    DebugCts
        { traceCallCount :: TraceCallCount
        -- ^ Trace TcPlugin call count.
        , traceCts :: TraceCts
        -- ^ Trace GHC constraints.
        , traceCarry :: TraceCarry
        -- ^ Trace GHC constraints carried through conversion and solving.
        , traceSolution :: TraceSolution
        -- ^ Trace the solution, the @TcPluginResult@.
        }