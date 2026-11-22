# A model's output of any kind on the chart: the output_spec document built by hand for the eleven kinds beside
# a path (a path is forecast.R's) - scalar, classes, quantiles, rank, position, weights, event, levels, action, text,
# series - checked as Bentrix checks it, a value outside its declared range refused in words and never clamped, and
# put through output_put; and the person's meaning (output_meaning) set through output_meaning_set, never in a model's
# file. An output never names its arrival: Bentrix stamps put_ms.
#
# A caller is anything with $call(name, args): the client, or a test's script.

#' The twelve kinds, as the document writes them.
#' @export
bx_output_kinds <- c("path", "scalar", "classes", "quantiles", "rank", "position", "weights", "event", "levels", "action", "text", "series")
#' The meaning's closed words.
#' @export
bx_meaning_words <- c("buy", "sell", "nothing", "up", "down", "flat", "in", "out", "close")
bx_output_intents <- c("buy", "sell", "close_buys", "close_sells", "nothing")

# A refusal in Bentrix's words, raised before anything is sent: a condition of class bx_output_refused.
bx_refuse <- function(msg) stop(structure(class = c("bx_output_refused", "error", "condition"), list(message = msg, call = NULL)))

bx_fmt <- function(v) if (is.numeric(v) && is.finite(v) && v == round(v) && abs(v) < 1e15) format(v, scientific = FALSE) else as.character(v)

bx_in_range <- function(field, v, range) {
  if (!is.numeric(v) || length(v) != 1 || !is.finite(v)) bx_refuse(paste(field, format(v), "is not a number"))
  if (!is.null(range) && (v < range[[1]] || v > range[[2]]))
    bx_refuse(sprintf("%s the output answers %s to %s; %s is outside it", field, bx_fmt(range[[1]]), bx_fmt(range[[2]]), bx_fmt(v)))
}

bx_named <- function(m) as.list(vapply(m, as.numeric, numeric(1)))

#' One output's document: kind, its values as the document writes them, and the kind's own fields.
#' @export
bx_output <- function(kind, model, name, symbol, anchor_ms, values, fields = list()) {
  self <- new.env()
  self$kind <- kind
  self$model <- model
  self$name <- name
  self$symbol <- symbol
  self$anchor_ms <- anchor_ms
  self$values <- values
  self$fields <- fields
  self$epoch <- 0
  self$suggested_meaning <- NULL
  # The horizon: steps = n, business_days = n, or until = "levels" | "next_output".
  self$horizon <- function(steps = NULL, business_days = NULL, until = NULL) {
    self$fields$horizon <- if (!is.null(steps)) list(steps = steps) else if (!is.null(business_days)) list(business_days = business_days) else list(until = until)
    invisible(self)
  }
  # Refuses what Bentrix would refuse, in its words.
  self$check <- function() {
    label <- "^[a-z0-9_]{1,32}$"
    if (!grepl(label, self$model) || !grepl(label, self$name)) bx_refuse("the model and the output are [a-z0-9_]{1,32}")
    if (!(self$kind %in% bx_output_kinds)) bx_refuse(paste(self$kind, "is not one of the twelve kinds"))
    if (!length(self$values)) bx_refuse("values holds no value")
    r <- self$fields$range
    names <- unlist(self$fields$names)
    softmax <- isTRUE(self$fields$softmax)
    for (i in seq_along(self$values)) {
      v <- self$values[[i]]
      f <- sprintf("values[%d]", i - 1)
      switch(self$kind,
        scalar = bx_in_range(paste0(f, ".value"), v$value, r),
        rank = bx_in_range(paste0(f, ".value"), v$value, if (is.null(r)) c(0, 1) else r),
        event = bx_in_range(paste0(f, ".score"), v$score, c(0, 1)),
        position = bx_in_range(paste0(f, ".target"), v$target, r),
        classes = {
          if (!length(names)) bx_refuse("names a classes output names its classes")
          for (k in names(v$p)) {
            if (!(k %in% names)) bx_refuse(paste0(f, ".p ", k, " is not one of the classes"))
            if (!softmax) bx_in_range(paste0(f, ".p.", k), v$p[[k]], c(0, 1))
          }
        },
        series = if (!length(names)) bx_refuse("names a series output names its lines"),
        action = if (!(isTRUE(v$intent %in% bx_output_intents))) bx_refuse(paste0(f, ".intent ", v$intent, " is not one of ", paste(bx_output_intents, collapse = ", "))),
        text = if (nchar(v$text, type = "chars") > 2000) bx_refuse(paste0(f, ".text is longer than 2000 characters")),
        NULL)
    }
    if (!is.null(self$suggested_meaning)) self$suggested_meaning$check(r)
    invisible(self)
  }
  # The output_spec document; never an arrival.
  self$spec <- function() {
    d <- list(schema_version = 1, model = self$model, output = self$name, symbol = self$symbol, kind = self$kind,
              anchor = list(time_ms = self$anchor_ms), values = self$values)
    for (k in names(self$fields)) d[[k]] <- self$fields[[k]]
    if (self$epoch > 0) d$epoch <- self$epoch
    if (!is.null(self$suggested_meaning)) d$suggested_meaning <- self$suggested_meaning$doc()
    d
  }
  # output_put on the workbench bench, after check.
  self$put <- function(caller, bench) {
    self$check()
    caller$call("output_put", list(bench = bench, output = self$spec()))
  }
  self
}

#' A number per step; range its declared bounds, c(lo, hi), or NULL.
#' @export
bx_output_scalar <- function(model, name, symbol, anchor_ms, values, range = NULL)
  bx_output("scalar", model, name, symbol, anchor_ms, lapply(values, function(v) list(value = as.numeric(v))), if (is.null(range)) list() else list(range = as.numeric(range)))
#' Probabilities over named classes, one named vector per step.
#' @export
bx_output_classes <- function(model, name, symbol, anchor_ms, names, steps, exclusive)
  bx_output("classes", model, name, symbol, anchor_ms, lapply(steps, function(p) list(p = bx_named(p))), list(names = as.list(names), exclusive = exclusive))
#' Quantiles at levels, one named vector per step keyed as the document writes a level ("0.1").
#' @export
bx_output_quantiles <- function(model, name, symbol, anchor_ms, levels, steps)
  bx_output("quantiles", model, name, symbol, anchor_ms, lapply(steps, function(q) list(q = bx_named(q))), list(levels = as.list(as.numeric(levels))))
#' A rank in 0..1 among universe markets.
#' @export
bx_output_rank <- function(model, name, symbol, anchor_ms, universe, value)
  bx_output("rank", model, name, symbol, anchor_ms, list(list(value = as.numeric(value))), list(universe = universe))
#' A target position in units (fraction, lots, ...).
#' @export
bx_output_position <- function(model, name, symbol, anchor_ms, target, units = "fraction")
  bx_output("position", model, name, symbol, anchor_ms, list(list(target = as.numeric(target))), list(units = units))
#' Weights across markets, a named vector.
#' @export
bx_output_weights <- function(model, name, symbol, anchor_ms, w, long_only = TRUE)
  bx_output("weights", model, name, symbol, anchor_ms, list(list(w = bx_named(w))), list(long_only = long_only, markets = as.list(names(w))))
#' An event's score in 0..1, expected within within_steps (NULL for none).
#' @export
bx_output_event <- function(model, name, symbol, anchor_ms, score, within_steps = NULL) {
  v <- list(score = as.numeric(score))
  if (!is.null(within_steps)) v$within_steps <- within_steps
  bx_output("event", model, name, symbol, anchor_ms, list(v))
}
#' Named price levels (stop, target, ...).
#' @export
bx_output_levels <- function(model, name, symbol, anchor_ms, levels)
  bx_output("levels", model, name, symbol, anchor_ms, list(list(levels = bx_named(levels))))
#' An intent, with a size where it has one.
#' @export
bx_output_action <- function(model, name, symbol, anchor_ms, intent, size = NULL) {
  v <- list(intent = intent)
  if (!is.null(size)) v$size <- as.numeric(size)
  bx_output("action", model, name, symbol, anchor_ms, list(v))
}
#' A sentence.
#' @export
bx_output_text <- function(model, name, symbol, anchor_ms, text) bx_output("text", model, name, symbol, anchor_ms, list(list(text = text)))
#' Named lines, one named vector per step.
#' @export
bx_output_series <- function(model, name, symbol, anchor_ms, names, steps)
  bx_output("series", model, name, symbol, anchor_ms, lapply(steps, function(l) list(lines = bx_named(l))), list(names = as.list(names)))

#' output_put_many: several outputs at one instant.
#' @export
bx_output_put_many <- function(caller, bench, outputs, all_or_nothing = FALSE) {
  specs <- lapply(outputs, function(o) { o$check(); o$spec() })
  caller$call("output_put_many", list(bench = bench, outputs = specs, all_or_nothing = all_or_nothing))
}

#' The output_meaning document: what a value means to the person.
#' @export
bx_meaning <- function(high_means = NULL, low_means = NULL, act_above = NULL, act_below = NULL, hysteresis = 0, reads = "direction", labels = list()) {
  self <- new.env()
  self$reads <- reads
  self$high_means <- high_means
  self$low_means <- low_means
  self$act_above <- act_above
  self$act_below <- act_below
  self$hysteresis <- hysteresis
  self$labels <- labels
  # Refuses a word that is not the product's, a threshold outside range, a label empty or over 24.
  self$check <- function(range = NULL) {
    for (w in list(self$high_means, self$low_means)) if (!is.null(w) && !(w %in% bx_meaning_words)) bx_refuse(sprintf("\"%s\" is not one of the meaning's words", w))
    if (!is.null(range)) for (t in list(self$act_above, self$act_below))
      if (!is.null(t) && (t < range[[1]] || t > range[[2]])) bx_refuse(sprintf("the output answers %s to %s; %s is outside it", bx_fmt(range[[1]]), bx_fmt(range[[2]]), bx_fmt(t)))
    for (k in names(self$labels)) {
      l <- self$labels[[k]]
      if (!(k %in% bx_meaning_words) || !nzchar(l) || nchar(l, type = "chars") > 24) bx_refuse(sprintf("the label \"%s\" for %s is not a meaning word's, or is empty or longer than 24", l, k))
    }
    invisible(self)
  }
  # The document.
  self$doc <- function() {
    d <- list(reads = self$reads, hysteresis = self$hysteresis)
    if (!is.null(self$high_means)) d$high_means <- self$high_means
    if (!is.null(self$low_means)) d$low_means <- self$low_means
    if (!is.null(self$act_above)) d$act_above <- self$act_above
    if (!is.null(self$act_below)) d$act_below <- self$act_below
    if (length(self$labels)) d$labels <- self$labels
    d
  }
  self
}

#' output_meaning_set: the person's meaning of model's output on the workbench bench.
#' @export
bx_output_meaning_set <- function(caller, bench, model, output, meaning, range = NULL) {
  meaning$check(range)
  caller$call("output_meaning_set", list(bench = bench, model = model, output = output, meaning = meaning$doc()))
}
