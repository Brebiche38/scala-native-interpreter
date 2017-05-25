#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "builtins.h"

extern char* data;

void* data_at(sn_ptr_t addr) {
	return (void*) (data + addr);
}

void scalanative_init(void) {
	return;
}

void* scalanative_alloc(sn_ptr_t rtti, int64_t size) { // TODO rtti really useful?
	printf("size: %lld, rtti %llx\n", size, rtti);
	sn_ptr_t* allocated = (sn_ptr_t*) malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	*allocated = rtti;

	printf("scalanative_alloc to %p (%lld bytes), rtti= %llx\n", allocated, size, allocated[0]);

	return allocated;
}

sn_ptr_t scalanative_field(sn_ptr_t obj, sn_rtti_t* rtti, int32_t id) {
	printf("obj at %llx, rtti at %p, id is %d", obj, rtti, id);
	int64_t* offsets = data_at(*((sn_ptr_t*) data_at(rtti->layout)));
	printf("field at %llx\n", obj + offsets[id] + 8);
	return obj + offsets[id] + 8;
}

sn_ptr_t scalanative_method_virtual(sn_ptr_t* obj, int32_t id) {
	sn_rtti_t* rtti = data_at(*obj);
	sn_ptr_t* vtable = data_at(rtti->vtable);
	return vtable[id];
}

sn_ptr_t scalanative_method_static(sn_ptr_t method) {
	return method; // TODO really? deref?
}

sn_ptr_t scalanative_method_trait(sn_type_t** obj, sn_ptr_t** dispatchTable, int32_t offset) {
	int32_t id = (*obj)->id;
	return dispatchTable[offset][id];
}

int8_t scalanative_is_class(sn_ptr_t* obj, sn_rtti_t *rtti) {
	sn_range_t* range = (sn_range_t*) (data + rtti->range);
	if (range->last - range->first == 0) {
		return rtti->type == *obj;
	} else {
		int32_t id = ((sn_type_t *) (data + *obj))->id;
		return (id > range->first && id < range->last);
	}
}

int8_t scalanative_is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
}

void* scalanative_alloc_raw(int64_t size) {
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	printf("Memory allocated to %p (%lld bytes)\n", allocated, size);

	return allocated;
}

void* scalanative_alloc_raw_atomic(int64_t size) {
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	printf("Memory atomically allocated to %p (%lld bytes)\n", allocated, size);

	return allocated;
}

void llvm_memset(int8_t* dest, int8_t val, int64_t len, int32_t align, int8_t is_volatile) {
	printf("Memsetting %p\n", dest);
	for (int i = 0; i < len; ++i) {
		dest[i] = val;
	}
}