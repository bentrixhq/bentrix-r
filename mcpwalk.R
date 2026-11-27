# The R MCP client's walk against a running Bentrix (bentrix --serve over the CI tape): it finds the
# application and pairs with nothing typed, reads its state through the generated surface, reads a
# resource by its template, and runs the same Gym episode every binding runs - through the agents'
# tools, over the wire - printed as JSON for sdk/python/tests/test_crosslang.py.
#
#   R CMD INSTALL --library=<lib> sdk/r/bentrix
#   LOCALAPPDATA=<the serve's> R_LIBS=<lib> Rscript sdk/r/mcpwalk.R
library(bentrix)

check <- function(held, what) if (!isTRUE(held)) {
  message("FAILED: ", what)
  quit(status = 1)
}

b <- connect("r-walk")
state <- b$workspace$get_state()
bench <- state$laboratories[[1]]$benches[[1]]$uri
p <- strsplit(sub("^bentrix://", "", bench), "/", fixed = TRUE)[[1]]
book <- b$trade$book(p[1], p[2], p[4])
check(is.list(book$positions), "the book did not read")
said <- tryCatch({
  b$call("no_such_tool")
  ""
}, bentrix_error = function(e) conditionMessage(e))
check(grepl("no_such_tool", said, fixed = TRUE), paste("a refusal did not say its tool:", said))

made <- b$strategy$env_make(bench, interval = "H1", window = 8, episode = "window", episode_bars = 12)
start <- b$strategy$env_reset(made$env, seed = 5)
check(length(start$obs$market) == 8, "the observation is not shaped as declared")
rewards <- numeric(0)
hashes <- start$info$ledger_hash
for (a in c(1, 1, 2, 0, 2, 2, 1, 0, 1, 2, 0, 1)) {
  s <- b$strategy$env_step(made$env, a)
  rewards <- c(rewards, s$reward)
  hashes <- c(hashes, s$info$ledger_hash)
  if (isTRUE(s$terminated) || isTRUE(s$truncated)) break
}
b$strategy$env_close(made$env)
# A tool's own refusal (isError), not a request the wire refused.
refusal <- tryCatch({
  b$strategy$env_step(made$env, 0)
  NULL
}, bentrix_tool_error = function(e) conditionMessage(e))
check(!is.null(refusal) && grepl("No environment", refusal, fixed = TRUE), paste("a closed environment's step was not refused in its words:", refusal))
check(length(unique(hashes)) > 1, "the episode traded nothing")
# Every reward at 17 significant digits reads back as the same double.
cat(sprintf(
  "{\"start_ms\": %s, \"rewards\": [%s], \"hashes\": [%s]}\n",
  format(start$info$start_ms, scientific = FALSE), paste(sprintf("%.17g", rewards), collapse = ", "),
  paste0("\"", hashes, "\"", collapse = ", ")
))
cat("PASS\n")
