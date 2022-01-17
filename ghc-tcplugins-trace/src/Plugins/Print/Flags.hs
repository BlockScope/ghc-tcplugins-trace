module Plugins.Print.Flags
    ( -- * Flags
      TraceCallCount(..)
    , TraceCts(..)
    , TraceSolution(..)
    , DebugCts(..)
      -- * Pretty Printing
    , pprSolverCallCount 
    ) where

import Plugins.Print.Constraints (Indent(..))

-- | Flag for controlling tracing counts of each time the plugin is called.
newtype TraceCallCount = TraceCallCount Bool

-- | Pretty print the calls of this plugin as a count if tracing calls.
pprSolverCallCount :: TraceCallCount -> String -> Indent -> Int -> String
pprSolverCallCount (TraceCallCount callCount) title (Indent i) n
    | callCount = let tab = showString $ replicate (2 * i) ' ' in
      ( showChar '['
      . showString title
      . showChar ']'
      . showChar '\n'
      . tab
      . showString "call = "
      . shows n)
      ""
    | otherwise = ""

-- | Flag for controlling tracing of type checking constraints.
newtype TraceCts = TraceCts Bool

-- | Flag for controlling tracing of the solution of type checking.
newtype TraceSolution = TraceSolution Bool

-- | A group of flags for tracing constraits.
data DebugCts =
    DebugCts
        { traceCallCount :: TraceCallCount
        -- ^ Trace TcPlugin call count.
        , traceCts :: TraceCts
        -- ^ Trace GHC constraints.
        , traceSolution :: TraceSolution
        -- ^ Trace the solution, the @TcPluginResult@.
        }