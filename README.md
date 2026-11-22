# Bentrix SDK for R

The R client of [Bentrix](https://bentrix.net) — charts, a replay laboratory and trading through your own MetaTrader — for a program or an agent.

It finds the Bentrix running on this computer through the agents folder Bentrix writes at every start, pairs with it under the name you give — the name you see in ☰ › Settings › **Agents**, where the pairing's permissions are set — and calls every tool of Bentrix's capability catalogue over MCP. Nothing is typed: no address, no key.

The client is generated from the catalogue of the Bentrix release it came with, and checks when it connects that the running Bentrix serves the same major version.

## What you need

R 4 or later with the packages `curl` and `jsonlite`, and Bentrix open on the same computer.

## Install

The client is the package `bentrixmcp`. From a clone of this repository:

```bash
R CMD INSTALL bentrixmcp
```

## Connect

```r
library(bentrixmcp)

b <- connect("my-agent")
state <- b$workspace$get_state()
bench <- state$laboratories[[1]]$benches[[1]]$uri
p <- strsplit(sub("^bentrix://", "", bench), "/", fixed = TRUE)[[1]]
book <- b$trade$book(p[1], p[2], p[4])
```

## The engine in your own process

The `bentrix` package and its Gym (the in-process engine) use Bentrix's engine library, which is not part of this repository.

## Documentation

Every method, its arguments, the permission it needs and what it answers: **https://bentrix.net/en/docs/sdk/r/**

## Licence

MIT — see [LICENSE](LICENSE). The licence covers this source code; Bentrix itself is not covered by it.
