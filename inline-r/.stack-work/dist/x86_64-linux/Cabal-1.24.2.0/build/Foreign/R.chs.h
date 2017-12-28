#define C2HS_MIN_VERSION(mj,mn,rv) ((mj)<=C2HS_VERSION_MAJOR && (mn)<=C2HS_VERSION_MINOR && (rv)<=C2HS_VERSION_REV)
#if __GLASGOW_HASKELL__ < 710
struct C2HS_COND_SENTRY_0;
#endif
#if __GLASGOW_HASKELL__ >= 710
struct C2HS_COND_SENTRY_1;
#endif
#if __GLASGOW_HASKELL__ < 710
struct C2HS_COND_SENTRY_2;
#endif
#define USE_RINTERNALS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Memory.h>
#include "missing_r.h"
const char *(R_CHAR)(SEXP x);


