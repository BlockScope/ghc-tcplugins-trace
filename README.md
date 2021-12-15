# `ghc-tcplugins-trace`

[![cabal](https://github.com/BlockScope/ghc-tcplugins-trace/actions/workflows/cabal.yml/badge.svg)](https://github.com/BlockScope/ghc-tcplugins-trace/actions/workflows/cabal.yml)
[![stack](https://github.com/BlockScope/ghc-tcplugins-trace/actions/workflows/stack.yml/badge.svg)](https://github.com/BlockScope/ghc-tcplugins-trace/actions/workflows/stack.yml)

Forked the tracing of the `thoralf-plugin` to create `ghc-tcplugins-trace`. It
can trace the call count, trace GHC constraints, trace constraints carried
through conversion and solving and trace the solution.

```haskell
DebugPlugin
    { traceCallCount = TraceCallCount False
    , traceCts = TraceCts False
    , traceCarry = TraceCarry False
    , traceSolution = TraceSolution False
    }
```

No automatic tracing going on here but when writing your own tracing, the record
of flags, the tracing functions and pretty printing functions are helpful.

## Similar Projects

[ghc-tcplugins-extra](https://github.com/clash-lang/ghc-tcplugins-extra) exports
`tcPluginTrace` that will:

> Print out extra information about the initialisation, stop, and every run of
the plugin when @-ddump-tc-trace@ is enabled