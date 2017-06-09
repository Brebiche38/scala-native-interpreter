#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "builtins.h"

void* data_at(sn_ptr_t addr) {
	return (void*) (data + addr);
}

sn_ptr_t scalanative_field(sn_ptr_t obj, sn_rtti_t* rtti, int32_t id) {
	uint64_t* offsets = data_at(rtti->layout);
	return obj + offsets[id];
}

sn_ptr_t scalanative_method_virtual(sn_ptr_t* obj, int32_t id) {
	if (obj == data) printf("Null pointer\n");
	sn_rtti_t* rtti = data_at(*obj);
	return rtti->vtable[id];
}

sn_ptr_t scalanative_method_static(sn_ptr_t method) {
	return method; // TODO really? deref?
}

sn_ptr_t scalanative_method_trait(sn_ptr_t* obj, sn_ptr_t* dispatchTable, int32_t methid, int32_t offset) {
	sn_type_t* type = data_at(*obj);
	return *(dispatchTable + offset + type->id);
}

int8_t scalanative_is_class(sn_ptr_t* obj, sn_rtti_t *ref_rtti) {
	if (obj == data) {
		return 0;
	}
	sn_rtti_t* obj_rtti = data_at(*obj);

	int8_t ret;
	if (ref_rtti->range.last - ref_rtti->range.first == 0) {
		ret = ref_rtti->type.id == obj_rtti->type.id;
	} else {
		ret = (obj_rtti->type.id >= ref_rtti->range.first && obj_rtti->type.id <= ref_rtti->range.last);
	}
	return ret;
}

int8_t scalanative_is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
}

int8_t scalanative_platform_is_windows() {
#ifdef _WIN32
    return 1;
#else
    return 0;
#endif
}

int64_t scalanative_strlen(char* str) {
	int len = 0;
	for(char* c = str; *c; ++c)
		len++;
	return len;
}

int scalanative_stdin_fileno() {
	return STDIN_FILENO;
}

int scalanative_stdout_fileno() {
	return STDOUT_FILENO;
}

int scalanative_stderr_fileno() {
	return STDERR_FILENO;
}