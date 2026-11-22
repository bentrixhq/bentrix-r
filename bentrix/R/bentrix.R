# BENTRIX for R: the engine in your own R session through the C ABI (crates/bentrix-ffi), its tools by
# the same names and shapes as over MCP - read, laboratory and paper, never a real account - and its
# Gym, a client of BENTRIX's own environment (env_make, env_reset, env_step).

# Numbers are written whole: jsonlite's default of four digits would turn a millisecond time into
# 1.7097e+12.
.json <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- Filter(Negate(is.null), x)
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null"))
}

.parse <- function(text) jsonlite::fromJSON(text, simplifyVector = FALSE)

#' Open a workspace (yours when NULL) and its recordings (yours when NULL).
#' @export
bx_local <- function(workspace = NULL, store = NULL) {
  structure(list(ptr = .Call(bxr_open, workspace, store)), class = "bentrix_local")
}

#' Close a session: the workspace persisted and let go.
#' @export
bx_close <- function(local) invisible(.Call(bxr_close, local$ptr))

#' One tool by name; its answer as a list, or its refusal raised in its own words.
#' @export
bx_call <- function(local, tool, args = NULL) .parse(.Call(bxr_call, local$ptr, tool, .json(args)))

#' One resource by its catalogue name, its template's variables.
#' @export
bx_read <- function(local, resource, vars = NULL) .parse(.Call(bxr_read, local$ptr, resource, .json(vars)))

#' Arm a workbench's engine, waiting up to max_frames.
#' @export
bx_arm <- function(local, bench, max_frames = 20000L) invisible(.Call(bxr_arm, local$ptr, bench, as.integer(max_frames)))

#' Advance a workbench to to_ms on its clock, as Play would; where it stands.
#' @export
bx_advance <- function(local, bench, to_ms) .Call(bxr_advance, local$ptr, bench, as.numeric(to_ms))

#' Frames until nothing waits, or max_frames.
#' @export
bx_settle <- function(local, max_frames = 1000L) invisible(.Call(bxr_settle, local$ptr, as.integer(max_frames)))

#' A workbench's open positions and pending orders.
#' @export
bx_book <- function(local, bench) {
  p <- strsplit(sub("bentrix://", "", bench, fixed = TRUE), "/", fixed = TRUE)[[1]]
  bx_read(local, "trade_book", list(home = p[1], owner = p[2], bench = p[4]))
}

#' The build of the engine loaded.
#' @export
bx_version <- function() .Call(bxr_version)

#' An interval's length in ms (M1, H4, D1); 0 for one with no fixed length.
#' @export
bx_interval_ms <- function(label) .Call(bxr_interval_ms, label)

#' The CI tape in directory: c(workspace, store).
#' @export
bx_make_tape <- function(directory) .Call(bxr_make_tape, directory)

#' The Gym: an environment over a workbench (env_make's arguments).
#' @export
bx_env <- function(local, bench, symbol = NULL, interval = "H1", window = 32, action = "discrete",
                   reward = "dsr_dd_penalty@1", lots = 0.01, episode = "window", episode_bars = 200,
                   participation = NULL, impact = NULL) {
  made <- bx_call(local, "env_make", list(
    bench = bench, symbol = symbol, interval = interval, window = window, action = action,
    reward = reward, lots = lots, episode = episode, episode_bars = episode_bars,
    participation = participation, impact = impact
  ))
  structure(list(local = local, id = made$env, symbol = made$symbol), class = "bentrix_env")
}

#' A new episode: list(obs, info). The seed given starts the draws over.
#' @export
bx_reset <- function(env, seed = NULL) {
  got <- bx_call(env$local, "env_reset", list(env = env$id, seed = seed))
  list(obs = got$obs, info = got$info)
}

#' One action: list(obs, reward, terminated, truncated, info).
#' @export
bx_step <- function(env, action) {
  got <- bx_call(env$local, "env_step", list(env = env$id, action = action))
  list(obs = got$obs, reward = got$reward, terminated = got$terminated, truncated = got$truncated, info = got$info)
}
