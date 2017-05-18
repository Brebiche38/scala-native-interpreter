#include <stdlib.h>
#include <stdint.h>

#include "builtins.h"

void scalanative_init(void) {
	return;
}

void* scalanative_alloc(sn_rtti_t* rtti, int64_t size) { // TODO rtti really useful?
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	return allocated;
}

sn_ptr_t scalanative_field(char* obj, sn_rtti_t* rtti, int32_t id) {
	return (sn_ptr_t) (obj + rtti->layout->offsets[id]);
}

sn_ptr_t scalanative_method_virtual(sn_rtti_t** obj, int32_t id) {
	sn_rtti_t* rtti = *obj;
	return rtti->vtable[id];
}

sn_ptr_t scalanative_method_static(sn_ptr_t method) {
	return method; // TODO really? deref?
}

sn_ptr_t scalanative_method_trait(sn_type_t** obj, void*** dispatchTable, int32_t offset) {
	int32_t id = (*obj)->id;
	return (sn_ptr_t) dispatchTable[offset][id];
}

int8_t scalanative_is_class(sn_type_t** obj, sn_rtti_t *rtti) {
	if (rtti->range->last - rtti->range->first == 0) {
		return rtti->type == *obj;
	} else {
		int32_t id = (*obj)->id;
		return (id > rtti->range->first && id < rtti->range->last);
	}
}

int8_t scalanative_is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
}