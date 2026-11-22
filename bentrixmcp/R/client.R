# The hand-written half of BENTRIX's R MCP client: the connection and everything that does not change
# when a capability is added (generated.R is the rest, from the catalogue). It speaks MCP revision
# 2026-07-28 over Streamable HTTP with curl: every request is its own POST carrying the protocol version,
# the client's identity and its (empty) capabilities in _meta - the same wire as the Python client's
# _runtime.py and the JavaScript client's client.js, field for field.
#
# connect() finds the running application through endpoint.json in BENTRIX's agents folder, pairs with
# it through local.secret under a name of this process, and connects with the token it gets.

.bx_protocol <- "2026-07-28"
# A call that asks the server to wait is given that long and 15 s more (found 2026-11-02: wait_for
# beyond the client's own timeout ended in the client before the server could answer).
.bx_long_poll_margin_s <- 15
.bx_client_name <- "bentrix-r-sdk"
.bx_client_version <- "0.1.0"
# **Busy, try again**: the JSON-RPC code Bentrix answers when it refused a request BEFORE doing anything
# with it -- one program's 20 calls a second spent, too many connections or streams, a task it could not
# read in time. Nothing ran, so the same request is sent again after the wait the answer names
# (retryAfterMs, a second when none), at most .bx_busy_retries times and each wait at most
# .bx_busy_wait_max_s; then the refusal is raised. An order is never sent twice by this.
.bx_busy <- -40001
.bx_busy_retries <- 10
.bx_busy_wait_max_s <- 5

.bx_busy_wait_s <- function(data) {
  ms <- if (is.list(data) && is.numeric(data$retryAfterMs) && length(data$retryAfterMs) == 1 && data$retryAfterMs > 0) data$retryAfterMs else 1000
  min(ms / 1000, .bx_busy_wait_max_s)
}

.bx_fail <- function(class, message) {
  stop(structure(class = c(class, "bentrix_error", "error", "condition"), list(message = message, call = NULL)))
}

# ---- JSON out: written here, so a number is written whole and a list argument stays an array ---------

# An array argument: a vector of one is still written as an array.
.bx_array <- function(x) if (is.null(x)) NULL else structure(as.list(x), class = "bx_array")

# An object, even when it is empty.
.bx_object <- function(x = list()) structure(x, class = "bx_object")

# The shortest of 15 or 17 significant digits that reads back as the same double.
.bx_num <- function(x) {
  if (!is.finite(x)) .bx_fail("bentrix_error", "JSON: a number that is not finite")
  if (x == trunc(x) && abs(x) < 1e15) return(format(x, scientific = FALSE, digits = 15))
  s <- sprintf("%.15g", x)
  if (as.numeric(s) != x) s <- sprintf("%.17g", x)
  s
}

.bx_str <- function(s) {
  s <- enc2utf8(as.character(s))
  for (p in list(c("\\", "\\\\"), c("\"", "\\\""), c("\n", "\\n"), c("\r", "\\r"), c("\t", "\\t"))) {
    s <- gsub(p[1], p[2], s, fixed = TRUE)
  }
  paste0("\"", s, "\"")
}

.bx_scalar_json <- function(x) {
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.integer(x)) return(as.character(x))
  if (is.numeric(x)) return(.bx_num(x))
  .bx_str(x)
}

.bx_json <- function(x) {
  if (is.null(x)) return("null")
  if (inherits(x, "bx_array")) return(paste0("[", paste(vapply(x, .bx_json, ""), collapse = ","), "]"))
  if (is.list(x)) {
    if (inherits(x, "bx_object") || !is.null(names(x))) {
      keep <- !vapply(x, is.null, logical(1))
      x <- x[keep]
      if (!length(x)) return("{}")
      return(paste0("{", paste(.bx_str(names(x)), vapply(x, .bx_json, ""), sep = ":", collapse = ","), "}"))
    }
    return(paste0("[", paste(vapply(x, .bx_json, ""), collapse = ","), "]"))
  }
  if (length(x) == 1) return(.bx_scalar_json(x))
  paste0("[", paste(vapply(as.list(x), .bx_scalar_json, ""), collapse = ","), "]")
}

.bx_parse <- function(text) jsonlite::fromJSON(text, simplifyVector = FALSE)

# Leave out what is NULL, so the server's own default applies.
.bx_plain <- function(args) {
  if (is.null(args)) return(.bx_object())
  .bx_object(args[!vapply(args, is.null, logical(1))])
}

# A header value, or the standard Base64 sentinel when plain ASCII cannot carry it.
.bx_header_value <- function(v) {
  bytes <- utf8ToInt(v)
  safe <- all(bytes >= 0x20 & bytes <= 0x7e) && v == trimws(v) && !(startsWith(v, "=?base64?") && endsWith(v, "?="))
  if (safe) v else paste0("=?base64?", jsonlite::base64_enc(charToRaw(enc2utf8(v))), "?=")
}

.bx_scalar <- function(v) {
  if (is.logical(v)) return(if (v) "true" else "false")
  if (is.numeric(v)) return(.bx_num(v))
  as.character(v)
}

# The RFC 6570 subset the catalogue uses: {name} and {?a,b}. A NULL optional is left out.
.bx_expand <- function(template, vars) {
  vars <- if (is.null(vars)) list() else vars[!vapply(vars, is.null, logical(1))]
  out <- template
  for (p in regmatches(template, gregexpr("\\{[?&]?[^}]+\\}", template))[[1]]) {
    inner <- substr(p, 2, nchar(p) - 1)
    op <- substr(inner, 1, 1)
    if (op %in% c("?", "&")) {
      ns <- strsplit(substring(inner, 2), ",")[[1]]
      ns <- ns[ns %in% names(vars)]
      enc <- vapply(ns, function(n) utils::URLencode(.bx_scalar(vars[[n]]), reserved = TRUE), "")
      rep <- if (length(ns)) paste0(op, paste0(ns, "=", enc, collapse = "&")) else ""
    } else {
      if (!(inner %in% names(vars))) .bx_fail("bentrix_error", paste0("the URI template ", template, " needs ", inner))
      rep <- utils::URLencode(.bx_scalar(vars[[inner]]), reserved = TRUE)
    }
    out <- sub(p, rep, out, fixed = TRUE)
  }
  out
}

.bx_post <- function(url, body, headers, timeout) {
  h <- curl::new_handle()
  curl::handle_setopt(h, post = TRUE, postfields = body, timeout = timeout)
  curl::handle_setheaders(h, .list = headers)
  r <- curl::curl_fetch_memory(url, handle = h)
  text <- rawToChar(r$content)
  Encoding(text) <- "UTF-8"
  list(status = r$status_code, type = if (is.null(r$type)) "" else r$type, text = text)
}

# The final response on a request-scoped event stream; notifications before it are ignored.
.bx_read_sse <- function(text, id) {
  data <- character(0)
  for (line in c(strsplit(text, "\n", fixed = TRUE)[[1]], "")) {
    line <- sub("\r$", "", line)
    if (line == "") {
      if (length(data)) {
        msg <- tryCatch(.bx_parse(paste(data, collapse = "\n")), error = function(e) NULL)
        if (!is.null(msg) && identical(msg$id, id) && (!is.null(msg$result) || !is.null(msg$error))) return(msg)
      }
      data <- character(0)
    } else if (startsWith(line, "data:")) {
      data <- c(data, sub("^ +", "", substring(line, 6)))
    }
  }
  .bx_fail("bentrix_error", "the stream ended without a response")
}

#' A client of one BENTRIX endpoint: every capability of its catalogue under b$<domain>, and call,
#' call_job, read_resource, read_pages, wait_for and verify_catalogue beside them.
#' @export
bentrix_client <- function(url, token, timeout = 60) {
  self <- new.env(parent = emptyenv())
  self$url <- url
  self$catalogue_version <- CATALOGUE_VERSION
  id <- 0

  # One request, sent again while Bentrix answers busy (.bx_busy): it ran nothing.
  rpc <- function(method, params, name = NULL, wait_s = timeout) {
    for (attempt in 0:.bx_busy_retries) {
      out <- rpc_once(method, params, name, wait_s)
      if (!isTRUE(out$refused)) return(out$result)
      if (isTRUE(out$code == .bx_busy) && attempt < .bx_busy_retries) {
        Sys.sleep(.bx_busy_wait_s(out$data))
        next
      }
      .bx_fail("bentrix_rpc_error", sprintf("[%s] %s", format(out$code, scientific = FALSE), out$message))
    }
  }

  # One request: list(result = ...), or list(refused = TRUE, code, message, data).
  rpc_once <- function(method, params, name, wait_s) {
    id <<- id + 1
    params[["_meta"]] <- list(
      `io.modelcontextprotocol/protocolVersion` = .bx_protocol,
      `io.modelcontextprotocol/clientInfo` = list(name = .bx_client_name, version = .bx_client_version),
      `io.modelcontextprotocol/clientCapabilities` = .bx_object()
    )
    body <- .bx_json(list(jsonrpc = "2.0", id = id, method = method, params = .bx_object(params)))
    headers <- c(
      `Content-Type` = "application/json", Accept = "application/json, text/event-stream",
      `MCP-Protocol-Version` = .bx_protocol, `Mcp-Method` = method, Authorization = paste("Bearer", token)
    )
    if (!is.null(name)) headers[["Mcp-Name"]] <- .bx_header_value(name)
    r <- .bx_post(url, body, headers, max(timeout, wait_s))
    msg <- if (startsWith(r$type, "text/event-stream")) {
      .bx_read_sse(r$text, id)
    } else {
      tryCatch(.bx_parse(r$text), error = function(e) .bx_fail("bentrix_rpc_error", sprintf("[%d] HTTP %d with no JSON-RPC body", r$status, r$status)))
    }
    if (!is.null(msg$error)) {
      code <- if (is.null(msg$error$code)) r$status else msg$error$code
      return(list(refused = TRUE, code = code, message = if (is.null(msg$error$message)) "" else msg$error$message, data = msg$error$data))
    }
    list(result = msg$result)
  }

  # A tool by its wire name; NULL arguments are left out.
  self$call <- function(name, args = NULL) {
    a <- .bx_plain(args)
    wait_s <- if (is.numeric(a$timeout_ms)) a$timeout_ms / 1000 + .bx_long_poll_margin_s else timeout
    res <- rpc("tools/call", list(name = name, arguments = a), name, wait_s)
    if (identical(res$resultType, "input_required")) {
      .bx_fail("bentrix_error", paste0(name, " needs input this client cannot give; answer it in Bentrix's window"))
    }
    text <- paste0(vapply(Filter(function(c) identical(c$type, "text"), res$content), function(c) c$text, ""), collapse = "")
    if (isTRUE(res$isError)) .bx_fail("bentrix_tool_error", paste0(name, ": ", if (nzchar(text)) text else "the tool failed"))
    if (!is.null(res$structuredContent)) return(res$structuredContent)
    if (nzchar(text)) tryCatch(.bx_parse(text), error = function(e) list()) else list()
  }

  # A long operation: a job whether the server answered at once or with a handle; job$wait() answers
  # its result once it completes (the person may act first).
  self$call_job <- function(name, args = NULL) {
    raw <- self$call(name, args)
    handle <- if (is.list(raw) && identical(names(raw), "job") && is.character(raw$job)) raw$job else NULL
    status <- function() if (is.null(handle)) list(status = "completed") else self$call("job_get", list(job = handle))
    list(
      handle = handle,
      status = status,
      cancel = function() if (!is.null(handle)) invisible(self$call("job_cancel", list(job = handle))),
      wait = function(timeout = NULL, poll = 0.5) {
        if (is.null(handle)) return(raw)
        deadline <- if (is.null(timeout)) Inf else Sys.time() + timeout
        repeat {
          st <- status()
          if (identical(st$status, "completed")) return(if (is.null(st$result)) list() else st$result)
          if (st$status %in% c("failed", "cancelled")) .bx_fail("bentrix_job_failed", sprintf("job %s %s: %s", handle, st$status, if (is.null(st$message)) "" else st$message))
          if (Sys.time() >= deadline) .bx_fail("bentrix_error", sprintf("job %s is still %s", handle, st$status))
          Sys.sleep(poll)
        }
      }
    )
  }

  read_uri <- function(uri) {
    res <- rpc("resources/read", list(uri = uri), uri)
    first <- if (length(res$contents)) res$contents[[1]] else NULL
    if (is.null(first) || !is.character(first$text)) .bx_fail("bentrix_error", paste0(uri, " answered no text content"))
    .bx_parse(first$text)
  }

  self$read_resource <- function(template, vars = NULL) read_uri(.bx_expand(template, vars))

  # Every page of a cursor-paged resource, as a list: next is the URI of the following page.
  self$read_pages <- function(template, vars = NULL) {
    pages <- list()
    uri <- .bx_expand(template, vars)
    while (!is.null(uri) && nzchar(uri)) {
      page <- read_uri(uri)
      pages[[length(pages) + 1]] <- page
      uri <- page$`next`
    }
    pages
  }

  # One wait on a stream: list(items, next); give next back as after to read on.
  self$wait_for <- function(template, vars = NULL, after = NULL, timeout_ms = 30000) {
    self$call("wait_for", list(uri = .bx_expand(template, vars), timeout_ms = timeout_ms, after = after))
  }

  # Refuse a server on another major line of the catalogue.
  self$verify_catalogue <- function() {
    server <- self$call("agent_whoami")$catalogue_version
    major <- function(v) {
      p <- strsplit(as.character(v), ".", fixed = TRUE)[[1]]
      if (length(p) != 3 || any(!grepl("^[0-9]+$", p))) .bx_fail("bentrix_incompatible_catalogue", paste0(v, " is not a version major.minor.patch"))
      as.integer(p[1])
    }
    if (major(server) != major(CATALOGUE_VERSION)) {
      .bx_fail("bentrix_incompatible_catalogue", sprintf("this SDK was generated from catalogue %s; the server's is %s", CATALOGUE_VERSION, server))
    }
    server
  }

  domains <- .bx_domains(self)
  for (d in names(domains)) assign(d, domains[[d]], envir = self)
  class(self) <- "bentrix_client"
  self
}

#' The folder BENTRIX writes its endpoint and secret into - user_dir(Data, "agents"), as the
#' application has it.
#' @export
agents_dir <- function() {
  lad <- Sys.getenv("LOCALAPPDATA")
  if (nzchar(lad)) return(file.path(lad, "BENTRIX", "agents"))
  home <- Sys.getenv("HOME")
  if (nzchar(home)) return(file.path(home, ".local", "share", "bentrix", "agents"))
  "bentrix-agents"
}

.bx_read_endpoint <- function(folder) {
  file <- file.path(folder, "endpoint.json")
  doc <- tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(doc)) .bx_fail("bentrix_not_running", paste0("Bentrix is not running: there is no readable endpoint.json in ", folder))
  if (!is.character(doc$url) || !startsWith(doc$url, "http://127.0.0.1:") || !is.numeric(doc$pid)) {
    .bx_fail("bentrix_not_running", paste0("Bentrix is not running: ", file, " is not an endpoint file"))
  }
  if (.Platform$OS.type == "unix" && !isTRUE(tools::pskill(doc$pid, 0L))) {
    .bx_fail("bentrix_not_running", sprintf("Bentrix is not running: %s is stale (process %s has ended)", file, format(doc$pid, scientific = FALSE)))
  }
  doc$url
}

.bx_read_secret <- function(folder) {
  file <- file.path(folder, "local.secret")
  if (!file.exists(file)) .bx_fail("bentrix_pairing_refused", paste0("there is no readable local.secret in ", folder, ": Bentrix writes it at every start"))
  secret <- trimws(paste(readLines(file, warn = FALSE), collapse = "\n"))
  if (!nzchar(secret)) .bx_fail("bentrix_pairing_refused", "local.secret is empty")
  secret
}

#' The name this process pairs under: BENTRIX_AGENT_NAME, else the script's name.
#' @export
default_name <- function() {
  env <- trimws(Sys.getenv("BENTRIX_AGENT_NAME"))
  if (nzchar(env)) return(env)
  file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(file)) paste0(.bx_client_name, " (", basename(file[1]), ")") else .bx_client_name
}

#' Present the secret to url/pair and answer the token. The secret travels in this one body.
#' @export
pair <- function(url, secret, name) {
  body <- .bx_json(list(secret = secret, client = list(name = name)))
  r <- .bx_post(paste0(sub("/$", "", url), "/pair"), body, c(`Content-Type` = "application/json"), 60)
  doc <- tryCatch(.bx_parse(r$text), error = function(e) list())
  if (r$status < 200 || r$status > 299) {
    code <- if (is.null(doc$error$code)) "" else doc$error$code
    if (identical(code, "wrong_secret")) .bx_fail("bentrix_pairing_refused", "Bentrix refused the secret in the secret file: it writes a new secret at every start")
    .bx_fail("bentrix_pairing_refused", sprintf("Bentrix answered the pairing request with HTTP %d %s: %s", r$status, code, if (is.null(doc$error$message)) "" else doc$error$message))
  }
  if (!is.character(doc$token) || !nzchar(doc$token)) .bx_fail("bentrix_pairing_refused", "Bentrix answered the pairing request with no token")
  doc$token
}

#' Find the running application, pair under name and connect: a bentrix_client.
#' @export
connect <- function(name = NULL, agents_folder = NULL) {
  folder <- if (is.null(agents_folder)) agents_dir() else agents_folder
  url <- .bx_read_endpoint(folder)
  bentrix_client(url, pair(url, .bx_read_secret(folder), if (is.null(name)) default_name() else name))
}
