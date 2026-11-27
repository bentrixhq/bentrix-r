# A forecast's builder and an evaluation's sheet.
#
#   Rscript sdk/r/bentrix/tests/forecast_test.R
#
# Mutations (2026-11-08), each red: spec writing the timeframe whatever it is (the element form named an interval);
# declined read as a failure (a decline was EvalFailed).
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(here, "..", "R", "forecast.R"))

failed <- 0
check <- function(ok, what) if (!isTRUE(ok)) { cat("FAIL:", what, "\n"); failed <<- failed + 1 }

sheet <- function(...) {
  s <- new.env()
  s$pages <- list(...)
  s$asked <- character(0)
  s$call <- function(name, args) {
    if (name != "wait_for" || args$uri != bx_eval_event) stop("not the eval event")
    s$asked <- c(s$asked, args$after)
    items <- if (length(s$pages)) s$pages[[1]] else list()
    if (length(s$pages)) s$pages <- s$pages[-1]
    list(items = items, `next` = as.character(as.integer(args$after) + length(items)))
  }
  s
}
moved <- function(ask, state, ...) c(list(ask = ask, state = state), list(...))

f <- bx_forecast_of_elements("bricks", "EURUSD", "renko", 1700000000000, elements = list(renko_box = 0.0005))
f$step(1.1005, expected_ms = 1700000600000)
d <- f$spec()
check(is.null(d$form$timeframe) && d$form$chart_type == "renko" && d$form$elements$renko_box == 0.0005, "the element form named an interval")
check(d$steps[[1]]$expected_ms == 1700000600000, "a step lost its expected_ms")
t <- bx_forecast("m", "EURUSD", "H1", 0)
t$step(1)
check(t$spec()$form$timeframe == "H1", "the time form named no interval")

check(bx_eval_started(sheet(), list(eval = 7), idle_limit = 1) == 7, "a run at once")
ask <- list(ask = "bentrix://ask/2", watch = "bentrix://forecasts/eval?after=0")
s <- sheet(list(moved("bentrix://ask/1", "declined"), moved("bentrix://ask/2", "asked")), list(moved("bentrix://ask/2", "made_laboratory")),
           list(moved("bentrix://ask/2", "started", eval = 11)))
check(bx_eval_started(s, ask, idle_limit = 3) == 11, "the run the person started")
check(identical(s$asked, c("0", "2", "3")), paste("the cursor", paste(s$asked, collapse = ",")))
got <- tryCatch({ bx_eval_started(sheet(list(moved("bentrix://ask/2", "declined"))), ask, idle_limit = 3); "run" },
                bentrix_declined_by_the_person = function(e) "declined", error = function(e) "failed")
check(got == "declined", "a decline was EvalFailed")
why <- tryCatch({ bx_eval_started(sheet(list(moved("bentrix://ask/2", "failed", why = "the laboratory was not made"))), ask, idle_limit = 3); "" },
                bentrix_eval_failed = function(e) e$why)
check(why == "the laboratory was not made", "a failure lost its why")
quiet <- tryCatch({ bx_eval_started(sheet(), ask, idle_limit = 2); "run" }, bentrix_declined_by_the_person = function(e) "declined", error = function(e) "ended")
check(quiet == "ended", "a watch with no answer did not end")
cat(if (failed == 0) "PASS\n" else sprintf("FAILED %d\n", failed))
quit(status = if (failed == 0) 0 else 1)
