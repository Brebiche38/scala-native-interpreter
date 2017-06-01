#include <libunwind.h>
#include <stdio.h>

int scalanative_unwind_get_context(void *context) {
	printf("in unwind_get_context\n");
	return 0;
    //return unw_getcontext((unw_context_t *)context);
}

int scalanative_unwind_init_local(void *cursor, void *context) {
	printf("in unwind_init_local\n");
    return 0;
    //return unw_init_local((unw_cursor_t *)cursor, (unw_context_t *)context);
}

int scalanative_unwind_step(void *cursor) {
	printf("in unwind_step\n");
    return 0;
    //return unw_step((unw_cursor_t *)cursor);
}

int scalanative_unwind_get_proc_name(void *cursor, char *buffer, size_t length,
                                     void *offset) {
	printf("in unwind_get_proc_name\n");
    return 0;
    //return unw_get_proc_name((unw_cursor_t *)cursor, buffer, length,
    //                         (unw_word_t *)offset);
}
