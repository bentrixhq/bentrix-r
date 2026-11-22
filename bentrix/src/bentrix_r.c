/*
 * bentrix_r.c - R's door to BENTRIX's C ABI (crates/bentrix-ffi/include/bentrix.h): each .Call entry
 * one ABI function, UTF-8 text in and out, a NULL or -1 raised as an R error in BENTRIX's own words.
 * A session is an external pointer closed by its finalizer, or by bx_close() explicitly.
 */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

#include "bentrix.h"

static const char *str(SEXP x) {
    if (!isString(x) || LENGTH(x) != 1) error("a single string was expected");
    return translateCharUTF8(STRING_ELT(x, 0));
}

static const char *str_or_null(SEXP x) {
    return isNull(x) ? NULL : str(x);
}

/* An answered char *, released; NULL raised as the failure it was. */
static SEXP take(char *p) {
    if (!p) error("%s", bx_last_error());
    SEXP s = PROTECT(mkCharCE(p, CE_UTF8));
    bx_free(p);
    SEXP v = ScalarString(s);
    UNPROTECT(1);
    return v;
}

static void done(int status) {
    if (status != 0) error("%s", bx_last_error());
}

static void finalize(SEXP p) {
    BxSession *s = (BxSession *)R_ExternalPtrAddr(p);
    if (s) {
        bx_close(s);
        R_ClearExternalPtr(p);
    }
}

static BxSession *session(SEXP p) {
    BxSession *s = (BxSession *)R_ExternalPtrAddr(p);
    if (!s) error("the session is closed");
    return s;
}

SEXP bxr_open(SEXP workspace, SEXP store) {
    BxSession *s = bx_open(str_or_null(workspace), str_or_null(store));
    if (!s) error("%s", bx_last_error());
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
    return take(bx_call(session(p), str(tool), str_or_null(args)));
}

SEXP bxr_read(SEXP p, SEXP resource, SEXP vars) {
    return take(bx_read(session(p), str(resource), str_or_null(vars)));
}

SEXP bxr_arm(SEXP p, SEXP bench, SEXP max_frames) {
    done(bx_arm(session(p), str(bench), (size_t)asInteger(max_frames)));
    return R_NilValue;
}

SEXP bxr_advance(SEXP p, SEXP bench, SEXP to_ms) {
    int64_t reached = 0;
    done(bx_advance(session(p), str(bench), (int64_t)asReal(to_ms), &reached));
    return ScalarReal((double)reached);
}

SEXP bxr_settle(SEXP p, SEXP max_frames) {
    done(bx_settle(session(p), (size_t)asInteger(max_frames)));
    return R_NilValue;
}

SEXP bxr_version(void) {
    return mkString(bx_version());
}

SEXP bxr_interval_ms(SEXP label) {
    return ScalarReal((double)bx_interval_ms(str(label)));
}

SEXP bxr_make_tape(SEXP directory) {
    char *ws = NULL, *store = NULL;
    done(bx_make_tape(str(directory), &ws, &store));
    SEXP out = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(out, 0, mkCharCE(ws, CE_UTF8));
    SET_STRING_ELT(out, 1, mkCharCE(store, CE_UTF8));
    bx_free(ws);
    bx_free(store);
    UNPROTECT(1);
    return out;
}

static const R_CallMethodDef calls[] = {
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
