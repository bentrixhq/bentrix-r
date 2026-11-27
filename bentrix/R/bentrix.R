# Bentrix for R: the engine in your own R session through the C ABI (crates/bentrix-ffi), its tools by
# the same names and shapes as over MCP - read, laboratory and paper, never a real account - and its
# Gym, a client of Bentrix's own environment (env_make, env_reset, env_step).

# The engine's library is opened on the engine's first use, never when the package loads, so the client
# (connect) needs none. It is looked for as every language's binding looks for it: the file BENTRIX_FFI
# names, libbentrix in the folder BENTRIX_FFI_DIR names, on Windows Bentrix's own folder
# (%LOCALAPPDATA%\Programs\BENTRIX, where the installer puts it), then the system's library path.
.bx_library_name <- function() {
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Windows")) "libbentrix.dll" else if (identical(sys, "Darwin")) "libbentrix.dylib" else "libbentrix.so"
}

.bx_engine_state <- new.env(parent = emptyenv())

.bx_engine <- function() {
  if (isTRUE(.bx_engine_state$loaded)) {
    return(invisible(TRUE))
  }
  name <- .bx_library_name()
  dir <- Sys.getenv("BENTRIX_FFI_DIR")
  own <- Sys.getenv("LOCALAPPDATA")
  candidates <- c(
    Sys.getenv("BENTRIX_FFI"),
    if (nzchar(dir)) file.path(dir, name),
    if (.Platform$OS.type == "windows" && nzchar(own)) file.path(own, "Programs", "BENTRIX", name),
    name
  )
  tried <- character()
  for (file in candidates[nzchar(candidates)]) {
    if (grepl("[/\\\\]", file) && !file.exists(file)) {
      tried <- c(tried, file)
      next
    }
    opened <- tryCatch({
      .Call(bxr_load, enc2utf8(file))
      TRUE
    }, error = function(e) {
      tried <<- c(tried, paste0(file, " (", conditionMessage(e), ")"))
      FALSE
    })
    if (opened) {
      .bx_engine_state$loaded <- TRUE
      return(invisible(TRUE))
    }
  }
  stop(sprintf(
    "Bentrix's engine library (%s) was not found. It is installed with Bentrix; or set BENTRIX_FFI to the file, or BENTRIX_FFI_DIR to its folder. Looked at: %s",
    name, paste(tried, collapse = "; ")
  ), call. = FALSE)
}

# The arguments as JSON, by the package's own writer (client.R) - the one the connection to a running Bentrix writes
# with: a number is written whole, what is NULL is left out, and an array argument stays an array (.bx_array). The
# workbench object hands the engine's door the same arguments it hands the connection, and another writer here could
# not write them: its first step stopped on *No method asJSON S3 class: bx_array*.
.json <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  .bx_json(.bx_plain(x))
}

.parse <- function(text) jsonlite::fromJSON(text, simplifyVector = FALSE)

#' Open a workspace (yours when NULL) and its recordings (yours when NULL).
#' @export
bx_local <- function(workspace = NULL, store = NULL) {
  .bx_engine()
  structure(list(ptr = .Call(bxr_open, workspace, store)), class = "bentrix_local")
}

#' Close a session: the workspace persisted and let go.
#' @export
bx_close <- function(local) invisible(.Call(bxr_close, local$ptr))

#' One tool by name; its answer as a list, or its refusal raised in its own words.
#' @export
bx_call <- function(local, tool, args = NULL) .parse(.Call(bxr_call, local$ptr, tool, .json(args)))

# The engine in this process as a workbench object's door (bx_bench, the client's): bx_bench(bx_door(local)).
bx_door <- function(local) list(call = function(name, args = NULL) bx_call(local, name, args))

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
bx_version <- function() {
  .bx_engine()
  .Call(bxr_version)
}

#' An interval's length in ms (M1, H4, D1); 0 for one with no fixed length.
#' @export
bx_interval_ms <- function(label) {
  .bx_engine()
  .Call(bxr_interval_ms, label)
}

#' The CI tape in directory: c(workspace, store).
#' @export
bx_make_tape <- function(directory) {
  .bx_engine()
  .Call(bxr_make_tape, directory)
}

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
