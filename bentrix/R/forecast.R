# A model's forecast on the chart, built by hand: the forecast_spec document step by step - a time form (an
# interval) or an element form (a chart built by price: renko, kagi, line_break, point_figure, range_bars - no
# interval, its rule in form$elements) - put through forecast_put; and an evaluation's sheet waited on:
# forecast_eval_make's answer is the run at once, or an ask the person answers in Bentrix's window, watched on the
# bentrix://forecasts/eval event. A forecast never names its arrival: Bentrix stamps put_ms.
#
# A caller is anything with $call(name, args): the client, or a test's script.

#' The event an ask's moves are carried on.
#' @export
bx_eval_event <- "bentrix://forecasts/eval"

#' A forecast to put on a workbench: the forecast_spec document, built step by step.
#'
#' `timeframe` is a time form's interval, NULL for an element form (a chart built by price), whose rule is `elements`
#' where it is not the chart's own. `anchor_ms` is the open of the last closed bar (or the moment of the last closed
#' element) it is made at.
#' @export
bx_forecast <- function(model, symbol, timeframe, anchor_ms, chart_type = "candlestick", elements = NULL, epoch = 0, note = NULL, look = NULL) {
  self <- new.env()
  self$model <- model
  self$symbol <- symbol
  self$timeframe <- timeframe
  self$anchor_ms <- anchor_ms
  self$chart_type <- chart_type
  self$elements <- elements
  self$epoch <- epoch
  self$note <- note
  self$look <- look
  self$steps <- list()
  # One more bar. bands as list(c(lo, hi), c(lo, hi, p)); expected_ms, on every step or none, places an element form
  # on a chart by time.
  self$step <- function(close, open = NULL, high = NULL, low = NULL, volume = NULL, bands = NULL, quantiles = NULL, samples = NULL, expected_ms = NULL) {
    s <- list(close = as.numeric(close))
    for (k in c("open", "high", "low", "volume", "expected_ms")) {
      v <- get(k)
      if (!is.null(v)) s[[k]] <- v
    }
    if (length(bands)) s$bands <- lapply(bands, function(b) if (length(b) > 2) list(lo = b[[1]], hi = b[[2]], p = b[[3]]) else list(lo = b[[1]], hi = b[[2]]))
    if (length(quantiles)) s$quantiles <- structure(as.list(as.numeric(quantiles)), names = names(quantiles))
    if (length(samples)) s$samples <- as.list(as.numeric(samples))
    self$steps[[length(self$steps) + 1]] <- s
    invisible(self)
  }
  # The forecast_spec document.
  self$spec <- function() {
    form <- list(chart_type = self$chart_type)
    if (!is.null(self$timeframe)) form$timeframe <- self$timeframe
    if (!is.null(self$elements)) form$elements <- self$elements
    d <- list(schema_version = 1, model = self$model, symbol = self$symbol, form = form, anchor = list(time_ms = self$anchor_ms),
              steps = self$steps, epoch = self$epoch)
    if (!is.null(self$note)) d$note <- self$note
    if (!is.null(self$look)) d$look <- self$look
    d
  }
  # forecast_put on the workbench bench: its arrival, its state, where it stands on each chart.
  self$put <- function(caller, bench) caller$call("forecast_put", list(bench = bench, forecast = self$spec()))
  self
}

#' A forecast of a chart built by price: no interval, `elements` its rule (NULL for the chart's own).
#' @export
bx_forecast_of_elements <- function(model, symbol, chart_type, anchor_ms, elements = NULL, ...) {
  bx_forecast(model, symbol, NULL, anchor_ms, chart_type = chart_type, elements = elements, ...)
}

#' forecast_put_many: several markets and models at one instant.
#' @export
bx_forecast_put_many <- function(caller, bench, forecasts, all_or_nothing = FALSE) {
  caller$call("forecast_put_many", list(bench = bench, forecasts = lapply(forecasts, function(f) f$spec()), all_or_nothing = all_or_nothing))
}

declined_by_the_person <- function(ask) {
  structure(class = c("bentrix_declined_by_the_person", "error", "condition"), list(message = paste("the person declined", ask), call = NULL, ask = ask))
}

eval_failed <- function(ask, why) {
  structure(class = c("bentrix_eval_failed", "error", "condition"), list(message = why, call = NULL, ask = ask, why = why))
}

#' The run to drive, from forecast_eval_make's answer: its eval at once, or - over days the workbench's laboratory does
#' not hold - the person's answer on the sheet, watched on bx_eval_event: started answers the run's id; declined
#' signals a `bentrix_declined_by_the_person` condition (not a failure); failed a `bentrix_eval_failed` one with its
#' why. No tool answers the sheet. `idle_limit` waits in a row with no move of this ask end the watch (never, NULL).
#' @export
bx_eval_started <- function(caller, made, timeout_ms = 60000, idle_limit = NULL) {
  if (!is.null(made$eval)) return(as.numeric(made$eval))
  ask <- made$ask
  uri <- if (is.null(made$watch)) bx_eval_event else made$watch
  cursor <- if (grepl("after=", uri, fixed = TRUE)) strsplit(strsplit(uri, "after=", fixed = TRUE)[[1]][2], "&", fixed = TRUE)[[1]][1] else "0"
  uri <- strsplit(uri, "[{?]")[[1]][1]
  idle <- 0
  while (is.null(idle_limit) || idle < idle_limit) {
    page <- caller$call("wait_for", list(uri = uri, after = cursor, timeout_ms = timeout_ms))
    if (!is.null(page$`next`)) cursor <- page$`next`
    moved <- FALSE
    for (item in page$items) {
      if (!identical(item$ask, ask)) next
      moved <- TRUE
      state <- item$state
      if (identical(state, "started")) return(as.numeric(item$eval))
      if (identical(state, "declined")) stop(declined_by_the_person(ask))
      if (identical(state, "failed")) stop(eval_failed(ask, if (is.null(item$why)) "the evaluation did not start" else item$why))
    }
    idle <- if (moved) 0 else idle + 1
  }
  stop(paste0(ask, ": no answer on the sheet"))
}
