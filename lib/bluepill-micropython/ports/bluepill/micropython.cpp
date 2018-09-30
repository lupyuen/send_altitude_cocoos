#include <bluepill.h>
#include <logger.h>
#include "py/mpconfig.h"
#include "micropython.h"

extern "C" void nlr_jump_fail(void *val) {
    debug_println("*** Error: nlr_jump_fail"); debug_flush();
    while (1);
}

extern "C" void NORETURN __fatal_error(const char *msg) {
    debug_print("*** Fatal Error: ");
    debug_println(msg ? msg : "(missing)");
    debug_flush();
    while (1);
}

extern "C" void MP_WEAK __assert_func(const char *file, int line, const char *func, const char *expr) {
    //  printf("Assertion '%s' failed, at file %s:%d\n", expr, file, line);
    debug_print("*** Assertion '");
    debug_print(expr ? expr : "(missing)");
    debug_print("' failed, at file ");
    debug_print(file ? file : "(missing)");
    debug_print(":");
    debug_print(line);
    debug_println(""); debug_flush();
    __fatal_error("Assertion failed");
}
