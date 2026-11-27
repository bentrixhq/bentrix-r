# The R Bench's walk against a running Bentrix (bentrix --serve over a practice tape): the common tasks - a refusal
# read as an answer, a step, a buy with a stop and a target, a modify, a part closed, close_all, the account and the
# deals - through bench.R, the layer generated from the Bench table. What it saw is printed as JSON for
# sdk/python/tests/test_bench.py, which holds it to the engine's own walk over the same tape.
#
#   R CMD INSTALL --library=<lib> sdk/r/bentrix
#   LOCALAPPDATA=<the serve's> R_LIBS=<lib> Rscript sdk/r/mcpbench.R              (a running Bentrix)
#   BENTRIX_PRACTICE=<a practice folder> R_LIBS=<lib> Rscript sdk/r/mcpbench.R    (the engine in this process)
library(bentrix)

check <- function(held, what) if (!isTRUE(held)) {
  message("FAILED: ", what)
  quit(status = 1)
}

# One walk, either door: the engine in this process over a practice folder, or the running Bentrix.
practice <- Sys.getenv("BENTRIX_PRACTICE")
b <- if (nzchar(practice)) NULL else connect("r-bench")
bench <- if (is.null(b)) bx_bench(bx_door(bx_local(file.path(practice, "ws"), file.path(practice, "store")))) else bx_bench(b)
check(identical(bench$markets(), "XAUUSD"), paste("the markets:", paste(bench$markets(), collapse = ",")))
refused <- bench$buy(0.001)
check(!refused$ok && identical(refused$kind, "volume_out_of_range"), "the refusal")
check(!is.null(refused$reason) && grepl("volume_min", refused$remedy, fixed = TRUE) && endsWith(refused$docs, "go/concept.volume/"), "the refusal's words")
first <- bench$step(interval = "H1")
check(!is.null(first) && first$time > 0 && first$close > 0, "the first step")
walk_group <- bench$group("walk")
check(is.numeric(walk_group) && walk_group == 1, "a group's number")
closed <- bench$bars(interval = "H1", limit = 5L)
check(is.null(names(closed)) && length(closed) == 1 && closed[[1]][[1]] == first$time, "the closed candles")
q <- bench$quote()
check(!is.null(q) && q$ask > q$bid, "the quote")
sl <- round(q$ask - 30.0)
tp <- round(q$ask + 30.0)
check(bench$buy(0.1, sl = sl, tp = tp)$ok, "the buy")
bench$step(interval = "H1")
ps <- bench$positions()
check(length(ps) == 1, "the positions")
p <- ps[[1]]
check(identical(p$side, "buy") && p$volume == 0.1 && p$sl == sl && p$tp == tp, "the position")
check(bench$modify(p, sl = sl + 5.0)$ok, "the modify")
moved <- bench$positions()[[1]]
check(moved$sl == sl + 5.0 && moved$tp == tp, "only what was named changes")
check(bench$close(moved, lots = 0.04)$ok, "the part closed")
bench$step(interval = "H1")
rest <- bench$positions()[[1]]
check(rest$volume == 0.06, "the rest")
check(bench$close_all()$ok, "close_all")
bench$step(interval = "H1")
check(length(bench$positions()) == 0, "a position stayed")
nothing_open <- bench$close_all()
check(nothing_open$ok && identical(as.numeric(nothing_open$raw$closed), 0), "close_all with nothing open")
account <- bench$account()
check(account$equity == account$balance && account$balance != 10000.0, "the account")
deals <- bench$deals()
entries <- vapply(deals, function(d) d$entry, "")
check(sum(entries == "in") == 1 && sum(entries == "out") == 2, paste("the deals:", paste(entries, collapse = ",")))
kept <- bench$keep("kept")
check(length(kept$positions()) == 0, "the kept workbench reads")
# Past the days' end, advance stands at their last moment, by either door.
walk_end <- bench$advance(4102444800000)
check(is.numeric(walk_end) && walk_end > 0 && walk_end < 4102444800000, paste("the days' end:", walk_end))
if (!is.null(b)) {
  # A workbench resource by its address: the same book as by its three parts.
  p <- strsplit(substring(bench$uri, nchar("bentrix://") + 1), "/", fixed = TRUE)[[1]]
  by_parts <- bentrix:::.bx_json(b$trade$book(p[1], p[2], p[4]))
  check(identical(bentrix:::.bx_json(b$trade$book(bench$uri)), by_parts), "the book by its address")
  check(identical(bentrix:::.bx_json(b$trade$book(bench)), by_parts), "the book by the workbench")
}
seen <- list(
  refused = list(refused$outcome, refused$kind, refused$reason),
  first = first,
  group = walk_group,
  bars = list(closed[[1]][[1]]),
  deals = lapply(deals, function(d) list(d$entry, d$side, d$volume)),
  balance = account$balance,
  end = walk_end
)
cat(bentrix:::.bx_json(seen), "\n", sep = "")
cat("PASS\n")
