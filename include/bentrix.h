/*
 * bentrix.h - BENTRIX's engine behind a C ABI (crates/bentrix-ffi).
 *
 * The embedded BENTRIX - the venue, the replay clock that cannot look ahead, the recordings, the Gym -
 * in your own process. JSON in, JSON out: a tool by name with its arguments, a resource by its
 * catalogue name with its variables - the agents' own catalogue, read, laboratory and paper.
 *
 * The contract:
 *   - A session is used from the thread that opened it, one thread at a time.
 *   - A function answering a char * gives you ownership: release it with bx_free. NULL is a failure,
 *     and bx_last_error() says why, on this thread, until the next failure on it.
 *   - A function answering an int answers 0 for done and -1 for a failure, with its sentence.
 *
 * Library: libbentrix.so (Linux), libbentrix.dll (Windows), libbentrix.dylib (macOS,
 * UNVERIFIED). This header is held to the library's exported functions by the crate's own test.
 */
#ifndef BENTRIX_H
#define BENTRIX_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct BxSession BxSession;

/* The build this library was made from. Static: never freed. */
const char *bx_version(void);

/* Why the last failure on this thread failed. Never freed by the caller. */
const char *bx_last_error(void);

/* Release a char * this library answered. NULL is a no-op. */
void bx_free(char *p);

/* Open a workspace (NULL: the person's own) and its recordings (NULL: the person's own). NULL on a
 * failure - a window or a --serve already on the workspace refuses it by name. */
BxSession *bx_open(const char *workspace, const char *store);

/* Close a session: the workspace persisted and let go. NULL is a no-op. */
void bx_close(BxSession *s);

/* One tool by name, its arguments a JSON object (NULL: none). Its answer as JSON, or NULL. */
char *bx_call(BxSession *s, const char *tool, const char *args_json);

/* One resource by its catalogue name, its variables a JSON object of strings. Its content, or NULL. */
char *bx_read(BxSession *s, const char *resource, const char *vars_json);

/* Arm a workbench's engine, turning up to max_frames frames until it is ready. */
int bx_arm(BxSession *s, const char *bench, size_t max_frames);

/* Advance a workbench to to_ms on its own clock, as Play would; where it stands in *reached. */
int bx_advance(BxSession *s, const char *bench, int64_t to_ms, int64_t *reached);

/* One frame: 1 when work is waiting, 0 when none, -1 on a failure. */
int bx_frame(BxSession *s);

/* Frames until nothing is waiting, or max_frames. */
int bx_settle(BxSession *s, size_t max_frames);

/* An interval's length in ms (M1, H4, D1, W1, S30); 0 for one with no fixed length. */
int64_t bx_interval_ms(const char *label);

/* The CI tape in directory: its workspace and store paths in the out-pointers (bx_free each). */
int bx_make_tape(const char *directory, char **workspace_out, char **store_out);

/* The candle of interval holding at_ms on the workbench's own cut: when it opens and closes, in UTC ms. */
int bx_candle_span(BxSession *s, const char *bench, const char *interval, int64_t at_ms, int64_t *opens, int64_t *closes);

/* One close of interval: the workbench advanced to it, each market's closed candle read. JSON (bx_free):
   {"reached": ms, "opened": ms, "bars": [[symbol, [open, high, low, close, volume, spread]], ...]}, or null when
   the close is past end_ms (given when has_end is not 0). symbols_json is a JSON array of symbols. */
char *bx_next_close(BxSession *s, const char *bench, const char *interval, const char *symbols_json, int64_t end_ms, int has_end);

/* Up to n closes, as bx_next_close each: a JSON array (bx_free). */
char *bx_run_until(BxSession *s, const char *bench, const char *interval, const char *symbols_json, size_t n, int64_t end_ms, int has_end);

/* Remember where an armed workbench stands; its number in id. */
int bx_snapshot(BxSession *s, const char *bench, uint64_t *id);

/* Take an armed workbench back to snapshot id; where its clock stands in reached (may be NULL). */
int bx_restore(BxSession *s, const char *bench, uint64_t id, int64_t *reached);

/* A tape of the markets named ([[symbol, scale], ...], the first the main one) over first..last (YYYY-MM-DD):
   its workspace and store paths in the out-pointers (bx_free each). */
int bx_make_markets(const char *directory, const char *first, const char *last, const char *markets_json, char **workspace_out, char **store_out);

#ifdef __cplusplus
}
#endif

#endif /* BENTRIX_H */
