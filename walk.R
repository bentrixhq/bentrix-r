# The R binding's walk on the CI tape - the engine (a second session refused, thirty hours advanced, a
# market order filled, the book read, a refusal raised in its words) and a Gym episode (seed 5, window
# 8, 12 candles, twelve fixed actions) printed as JSON for sdk/python/tests/test_crosslang.py.
#
#   Rscript sdk/r/walk.R <empty directory>      (the package installed: R CMD INSTALL sdk/r/bentrix)

library(bentrix)

check <- function(held, what) if (!isTRUE(held)) stop(paste("FAILED:", what))

dir <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(dir)) stop("usage: walk.R <empty directory>")
cat("bentrix", bx_version(), "\n")
paths <- bx_make_tape(dir)
local <- bx_local(paths[1], paths[2])
refused <- tryCatch({ bx_local(paths[1], paths[2]); NULL }, error = function(e) conditionMessage(e))
check(!is.null(refused), "a second session on the held workspace was not refused")
cat("a second session refused:", refused, "\n")
bench <- bx_call(local, "get_state")$laboratories[[1]]$benches[[1]]$uri
bx_arm(local, bench)
from <- bx_call(local, "replay_step", list(bench = bench, unit = "tick", n = 1))$cursor_ms
reached <- bx_advance(local, bench, from + 30 * 3600000)
check(reached >= from + 30 * 3600000, "the workbench did not advance")
invisible(bx_call(local, "order_market", list(bench = bench, symbol = "XAUUSD", side = "buy", volume = 0.01)))
reached <- bx_advance(local, bench, reached + 60000)
positions <- bx_book(local, bench)$positions
check(length(positions) == 1 && positions[[1]]$side == "buy", "the order did not fill")
said <- tryCatch({ bx_call(local, "no_such_tool"); "" }, error = function(e) conditionMessage(e))
check(grepl("no_such_tool", said, fixed = TRUE), paste("a refusal did not say its tool:", said))
check(bx_interval_ms("H1") == 3600000, "H1 is not an hour")

env <- bx_env(local, bench, interval = "H1", window = 8, episode = "window", episode_bars = 12)
actions <- c(1, 1, 2, 0, 2, 2, 1, 0, 1, 2, 0, 1)
start <- bx_reset(env, seed = 5)
check(length(start$obs$market) == 8 && length(start$obs$account) == 3, "the observation is not shaped as declared")
rewards <- c()
hashes <- c(start$info$ledger_hash)
for (a in actions) {
  s <- bx_step(env, a)
  rewards <- c(rewards, s$reward)
  hashes <- c(hashes, s$info$ledger_hash)
  if (isTRUE(s$terminated) || isTRUE(s$truncated)) break
}
check(length(unique(hashes)) > 1, "the episode traded nothing")
bx_close(local)
# Every number written whole: a reward to 17 significant digits reads back as the same double.
cat(sprintf('{"start_ms": %.0f, "rewards": [%s], "hashes": [%s]}\n', start$info$start_ms,
            paste(sprintf("%.17g", rewards), collapse = ", "), paste0('"', hashes, '"', collapse = ", ")))
cat("PASS\n")
