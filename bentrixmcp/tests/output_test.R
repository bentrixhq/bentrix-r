# Model outputs of every kind and the person's meaning: every kind builds a document; a value outside its
# declared range is refused in words before anything is sent; three examples are three meanings; a put
# never sends an arrival.
#
#   Rscript sdk/r/bentrixmcp/tests/output_test.R
#
# Mutation (2026-11-11), red: bx_in_range not comparing - a 1.3 for a 0..1 output was sent.
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(here, "..", "R", "output.R"))

failed <- 0
check <- function(ok, what) if (!isTRUE(ok)) { cat("FAIL:", what, "\n"); failed <<- failed + 1 }

rec <- function() {
  r <- new.env()
  r$names <- character(0)
  r$args <- list()
  r$call <- function(name, args) {
    r$names <- c(r$names, name)
    r$args[[length(r$args) + 1]] <- args
    list(ok = TRUE)
  }
  r
}
refused <- function(expr, needle = "") {
  tryCatch({ force(expr); FALSE }, bx_output_refused = function(e) !nzchar(needle) || grepl(needle, conditionMessage(e), fixed = TRUE))
}

built <- list(
  bx_output_scalar("m1", "o1", "EURUSD", 1, 0.7, range = c(0, 1)),
  bx_output_classes("m1", "o1", "EURUSD", 1, c("up", "down"), list(c(up = 0.6, down = 0.4)), exclusive = TRUE),
  bx_output_quantiles("m1", "o1", "EURUSD", 1, c(0.1, 0.9), list(c(`0.1` = 1.0, `0.9` = 1.2))),
  bx_output_rank("m1", "o1", "AAPL", 1, 4871, 0.62),
  bx_output_position("m1", "o1", "EURUSD", 1, -0.5),
  bx_output_weights("m1", "o1", "EURUSD", 1, c(EURUSD = 0.6, GBPUSD = 0.4)),
  bx_output_event("m1", "o1", "EURUSD", 1, 0.8, within_steps = 3),
  bx_output_levels("m1", "o1", "EURUSD", 1, c(stop = 1.09, target = 1.12)),
  bx_output_action("m1", "o1", "EURUSD", 1, "buy", size = 0.5),
  bx_output_text("m1", "o1", "EURUSD", 1, "the trend held"),
  bx_output_series("m1", "o1", "EURUSD", 1, "a", list(c(a = 1)))
)
kinds <- "path"
for (o in built) {
  o$check()
  check(identical(o$spec()$kind, o$kind), paste(o$kind, "wrote another kind"))
  kinds <- union(kinds, o$kind)
}
check(length(bx_output_kinds) == 12 && length(kinds) == 12, sprintf("the builders cover %d of %d kinds", length(kinds), length(bx_output_kinds)))

r <- rec()
check(refused(bx_output_scalar("m1", "o1", "EURUSD", 1, 1.3, range = c(0, 1))$put(r, "bench"), "0 to 1; 1.3 is outside it"), "a 1.3 for a 0..1 output was sent")
check(length(r$names) == 0, "something was sent")
check(refused(bx_output_rank("m1", "o1", "AAPL", 1, 10, 1.5)$check()), "a rank of 1.5 was taken")

m <- rec()
for (mm in list(bx_meaning("buy", "sell", 0.6, 0.4), bx_meaning("buy", "nothing", 0.6), bx_meaning("sell", "nothing", 0.6)))
  invisible(bx_output_meaning_set(m, "bench", "m1", "o1", mm, range = c(0, 1)))
check(m$names[[3]] == "output_meaning_set" && m$args[[3]]$meaning$high_means == "sell", "the meaning was not sent as set")
check(refused(bx_output_meaning_set(m, "bench", "m1", "o1", bx_meaning("profit"), range = c(0, 1))), "profit was taken as a meaning's word")
check(refused(bx_output_meaning_set(m, "bench", "m1", "o1", bx_meaning("buy", act_above = 1.5), range = c(0, 1))), "a threshold outside the range was taken")

p <- rec()
invisible(bx_output_scalar("m1", "dir", "EURUSD", 1, 0.7, range = c(0, 1))$horizon(steps = 4)$put(p, "bench"))
invisible(bx_output_put_many(p, "bench", list(bx_output_event("m1", "e", "EURUSD", 1, 0.9)), all_or_nothing = TRUE))
spec <- p$args[[1]]$output
check(p$names[[1]] == "output_put" && identical(spec$horizon$steps, 4), "output_put sent the wrong horizon")
check(is.null(spec$put_ms), "an arrival was sent")
check(p$names[[2]] == "output_put_many" && isTRUE(p$args[[2]]$all_or_nothing), "output_put_many sent the wrong flag")

cat(if (failed == 0) "output_test: all passed\n" else sprintf("output_test: %d failed\n", failed))
quit(status = if (failed == 0) 0 else 1)
