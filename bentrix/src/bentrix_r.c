/*
 * bentrix_r.c - R's door to Bentrix's C ABI (crates/bentrix-ffi/include/bentrix.h): each .Call entry
 * one ABI function, UTF-8 text in and out, a NULL or -1 raised as an R error in Bentrix's own words.
 * A session is an external pointer closed by its finalizer, or by bx_close() explicitly.
 *
 * The library is opened at RUN time (bxr_load, from R/bentrix.R's .bx_engine on the engine's first
 * use), never linked at install: the package installs with no libbentrix and no header, and its MCP
 * client never needs either. So the functions are reached through pointers whose types are the
 * header's own declarations, restated here (tests_agents's the_r_shim_restates_the_headers_own_
 * declarations holds the two to each other).
 */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

typedef struct BxSession BxSession;

/* The engine's interface this package speaks (bx_abi, bentrix.h's BX_ABI). */
#define BXR_ABI 2

/* bentrix.h's declarations, as pointer types. */
typedef const char *(*bx_version_fn)(void);
typedef const char *(*bx_last_error_fn)(void);
typedef void (*bx_free_fn)(char *p);
typedef BxSession *(*bx_open_fn)(const char *workspace, const char *store, const char *client);
typedef void (*bx_close_fn)(BxSession *s);
typedef char *(*bx_call_fn)(BxSession *s, const char *tool, const char *args_json);
typedef char *(*bx_read_fn)(BxSession *s, const char *resource, const char *vars_json);
typedef int (*bx_arm_fn)(BxSession *s, const char *bench, size_t max_frames);
typedef int (*bx_advance_fn)(BxSession *s, const char *bench, int64_t to_ms, int64_t *reached);
typedef int (*bx_settle_fn)(BxSession *s, size_t max_frames);
typedef int64_t (*bx_interval_ms_fn)(const char *label);
typedef int (*bx_make_tape_fn)(const char *directory, char **workspace_out, char **store_out);

static struct {
    int loaded;
    bx_version_fn version;
    bx_last_error_fn last_error;
    bx_free_fn free;
    bx_open_fn open;
    bx_close_fn close;
    bx_call_fn call;
    bx_read_fn read;
    bx_arm_fn arm;
    bx_advance_fn advance;
    bx_settle_fn settle;
    bx_interval_ms_fn interval_ms;
    bx_make_tape_fn make_tape;
} bx;

#ifdef _WIN32
static void *open_library(const char *file) {
    int n = MultiByteToWideChar(CP_UTF8, 0, file, -1, NULL, 0);
    if (n <= 0) return NULL;
    wchar_t *wide = (wchar_t *)malloc((size_t)n * sizeof(wchar_t));
    if (!wide) return NULL;
    MultiByteToWideChar(CP_UTF8, 0, file, -1, wide, n);
    HMODULE h = LoadLibraryW(wide);
    free(wide);
    return (void *)h;
}
static void *symbol(void *h, const char *name) { return (void *)GetProcAddress((HMODULE)h, name); }
static void opening_failed(const char *file) { error("Bentrix's engine library could not be opened: %s (error %lu)", file, (unsigned long)GetLastError()); }
#else
static void *open_library(const char *file) { return dlopen(file, RTLD_NOW | RTLD_LOCAL); }
static void *symbol(void *h, const char *name) { return dlsym(h, name); }
static void opening_failed(const char *file) {
    const char *why = dlerror();
    error("Bentrix's engine library could not be opened: %s (%s)", file, why ? why : "no reason given");
}
#endif

/* Opens the library at `file` once; every later call answers TRUE without opening it again. */
SEXP bxr_load(SEXP file) {
    if (bx.loaded) return ScalarLogical(TRUE);
    if (!isString(file) || LENGTH(file) != 1) error("a single string was expected");
    const char *path = translateCharUTF8(STRING_ELT(file, 0));
    void *h = open_library(path);
    if (!h) opening_failed(path);
#define BX_FIND(field, name)                                                                     \
    do {                                                                                          \
        *(void **)(&bx.field) = symbol(h, name);                                                  \
        if (!bx.field) error("Bentrix's engine library at %s does not hold %s: it is not this package's engine", path, name); \
    } while (0)
    BX_FIND(version, "bx_version");
    BX_FIND(last_error, "bx_last_error");
    BX_FIND(free, "bx_free");
    BX_FIND(open, "bx_open");
    BX_FIND(close, "bx_close");
    BX_FIND(call, "bx_call");
    BX_FIND(read, "bx_read");
    BX_FIND(arm, "bx_arm");
    BX_FIND(advance, "bx_advance");
    BX_FIND(settle, "bx_settle");
    BX_FIND(interval_ms, "bx_interval_ms");
    BX_FIND(make_tape, "bx_make_tape");
#undef BX_FIND
    /* **The engine's interface this package speaks** (bx_abi): a library of
     * another one is refused here, before bx_open, in a sentence naming both; one from before bx_abi (0.1.382) is 1. */
    {
        unsigned int (*abi)(void) = NULL;
        *(void **)(&abi) = symbol(h, "bx_abi");
        unsigned int have = abi ? abi() : 1;
        if (have != BXR_ABI)
            error("Bentrix's engine library speaks interface %u (Bentrix %s) and this package speaks %u: %s.", have, bx.version(),
                  (unsigned int)BXR_ABI, have < BXR_ABI ? "update Bentrix" : "update this package");
    }
    bx.loaded = 1;
    return ScalarLogical(TRUE);
}

static void engine(void) {
    if (!bx.loaded) error("Bentrix's engine library is not loaded");
}

static const char *str(SEXP x) {
    if (!isString(x) || LENGTH(x) != 1) error("a single string was expected");
    return translateCharUTF8(STRING_ELT(x, 0));
}

static const char *str_or_null(SEXP x) {
    return isNull(x) ? NULL : str(x);
}

/* An answered char *, released; NULL raised as the failure it was. */
static SEXP take(char *p) {
    if (!p) error("%s", bx.last_error());
    SEXP s = PROTECT(mkCharCE(p, CE_UTF8));
    bx.free(p);
    SEXP v = ScalarString(s);
    UNPROTECT(1);
    return v;
}

static void done(int status) {
    if (status != 0) error("%s", bx.last_error());
}

static void finalize(SEXP p) {
    BxSession *s = (BxSession *)R_ExternalPtrAddr(p);
    if (s && bx.loaded) {
        bx.close(s);
        R_ClearExternalPtr(p);
    }
}

static BxSession *session(SEXP p) {
    engine();
    BxSession *s = (BxSession *)R_ExternalPtrAddr(p);
    if (!s) error("the session is closed");
    return s;
}

SEXP bxr_open(SEXP workspace, SEXP store) {
    engine();
    BxSession *s = bx.open(str_or_null(workspace), str_or_null(store), "R");
    if (!s) error("%s", bx.last_error());
    SEXP p = PROTECT(R_MakeExternalPtr(s, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(p, finalize, TRUE);
    UNPROTECT(1);
    return p;
}

SEXP bxr_close(SEXP p) {
    finalize(p);
    return R_NilValue;
}

SEXP bxr_call(SEXP p, SEXP tool, SEXP args) {
    return take(bx.call(session(p), str(tool), str_or_null(args)));
}

SEXP bxr_read(SEXP p, SEXP resource, SEXP vars) {
    return take(bx.read(session(p), str(resource), str_or_null(vars)));
}

SEXP bxr_arm(SEXP p, SEXP bench, SEXP max_frames) {
    done(bx.arm(session(p), str(bench), (size_t)asInteger(max_frames)));
    return R_NilValue;
}

SEXP bxr_advance(SEXP p, SEXP bench, SEXP to_ms) {
    int64_t reached = 0;
    done(bx.advance(session(p), str(bench), (int64_t)asReal(to_ms), &reached));
    return ScalarReal((double)reached);
}

SEXP bxr_settle(SEXP p, SEXP max_frames) {
    done(bx.settle(session(p), (size_t)asInteger(max_frames)));
    return R_NilValue;
}

SEXP bxr_version(void) {
    engine();
    return mkString(bx.version());
}

SEXP bxr_interval_ms(SEXP label) {
    engine();
    return ScalarReal((double)bx.interval_ms(str(label)));
}

SEXP bxr_make_tape(SEXP directory) {
    engine();
    char *ws = NULL, *store = NULL;
    done(bx.make_tape(str(directory), &ws, &store));
    SEXP out = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(out, 0, mkCharCE(ws, CE_UTF8));
    SET_STRING_ELT(out, 1, mkCharCE(store, CE_UTF8));
    bx.free(ws);
    bx.free(store);
    UNPROTECT(1);
    return out;
}

static const R_CallMethodDef calls[] = {
    {"bxr_load", (DL_FUNC)&bxr_load, 1},
    {"bxr_open", (DL_FUNC)&bxr_open, 2},
    {"bxr_close", (DL_FUNC)&bxr_close, 1},
    {"bxr_call", (DL_FUNC)&bxr_call, 3},
    {"bxr_read", (DL_FUNC)&bxr_read, 3},
    {"bxr_arm", (DL_FUNC)&bxr_arm, 3},
    {"bxr_advance", (DL_FUNC)&bxr_advance, 3},
    {"bxr_settle", (DL_FUNC)&bxr_settle, 2},
    {"bxr_version", (DL_FUNC)&bxr_version, 0},
    {"bxr_interval_ms", (DL_FUNC)&bxr_interval_ms, 1},
    {"bxr_make_tape", (DL_FUNC)&bxr_make_tape, 1},
    {NULL, NULL, 0}};

void R_init_bentrix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
