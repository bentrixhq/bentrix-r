# Bentrix SDK for R

The R client of [Bentrix](https://bentrix.net) — charts, a replay laboratory and trading through your own MetaTrader — for a program or an agent.

It finds the Bentrix running on this computer through the agents folder Bentrix writes at every start, pairs with it under the name you give — the name you see in ☰ › Settings › **Agents**, where the pairing's permissions are set — and calls every tool of Bentrix's capability catalogue over MCP. Nothing is typed: no address, no key.

The client is generated from the catalogue of the Bentrix release it came with, and checks when it connects that the running Bentrix serves the same major version.

## What you need

R 4 or later with the packages `curl` and `jsonlite`, and Bentrix open on the same computer. Installed from source the package compiles a small C file, so it needs R's own compiler tools (Rtools on Windows) — as every R package with compiled code does.

## Install

One package, `bentrix`, holds the client and the engine. From a clone of this repository:

```bash
R CMD INSTALL bentrix
```

It installs with no engine library: the engine's is opened on its first use, never at install.

## Connect

```r
library(bentrix)

b <- connect("my-agent")
state <- b$workspace$get_state()
bench <- state$laboratories[[1]]$benches[[1]]$uri
p <- strsplit(sub("^bentrix://", "", bench), "/", fixed = TRUE)[[1]]
book <- b$trade$book(p[1], p[2], p[4])
```

## The engine in your own process

The same package's `bx_local` and its Gym (`bx_env`) run Bentrix's engine inside your program, with no window. The engine is Bentrix's own library, `libbentrix`, which Bentrix's installer puts in its folder (`%LOCALAPPDATA%\Programs\BENTRIX\libbentrix.dll` on Windows) and the binding finds there; `BENTRIX_FFI` (the file) or `BENTRIX_FFI_DIR` (its folder) names another. It runs for a computer signed in to Bentrix with Plus or Pro; with Free, connect to the running application as above.

## Documentation

Every method, its arguments, the permission it needs and what it answers: **https://bentrix.net/en/docs/sdk/r/**

## Licence

MIT — see [LICENSE](https://github.com/bentrixhq/bentrix-r/blob/main/LICENSE). It covers this source code only: Bentrix's engine library (`libbentrix`) and the Bentrix application are Bentrix's own, under their own terms.
