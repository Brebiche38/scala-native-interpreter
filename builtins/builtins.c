#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "builtins.h"

extern char* data;

void* data_at(sn_ptr_t addr) {
	return (void*) (data + addr);
}

/*
void scalanative_init(void) {
	return;
}

void* scalanative_alloc(sn_ptr_t rtti, int64_t size) {
	printf("size: %lld, rtti %llx\n", size, rtti);
	sn_ptr_t* allocated = (sn_ptr_t*) malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	*allocated = rtti;

	printf("scalanative_alloc to %p (%lld bytes), rtti= %llx\n", allocated, size, allocated[0]);

	return allocated;
}
*/

sn_ptr_t scalanative_field(sn_ptr_t obj, sn_rtti_t* rtti, int32_t id) {
	printf("obj at %llx, rtti at %p, id is %d\n", obj, rtti, id);
	uint64_t* offsets = data_at(rtti->layout);
	printf("offsets at %p\n", offsets);
	printf("field at %llx\n", obj + offsets[id]);
	return obj + offsets[id];
}

sn_ptr_t scalanative_method_virtual(sn_ptr_t* obj, int32_t id) {
	printf("Getting virtual method from obj %p at id %d, rtti is %llx\n", obj, id, *obj);
	sn_rtti_t* rtti = data_at(*obj);
	printf("Method is at %llx\n", rtti->vtable[id]);
	return rtti->vtable[id];
}

sn_ptr_t scalanative_method_static(sn_ptr_t method) {
	return method; // TODO really? deref?
}

sn_ptr_t scalanative_method_trait(sn_ptr_t* obj, sn_ptr_t* dispatchTable, int32_t offset) {
	printf("Getting trait method from obj %p at offset %d\n", obj, offset);
	sn_type_t* type = data_at(*obj);
	printf("Method ptr is at %p, id is %d\n", dispatchTable + offset + type->id, type->id);
	printf("Method is at %llx\n", *(dispatchTable + offset + type->id));
	return *(dispatchTable + offset + type->id);
}

int8_t scalanative_is_class(sn_ptr_t* obj, sn_rtti_t *ref_rtti) {
	if (obj == data) {
		printf("Null pointer\n");
		return 0;
	}
	printf("Object is at %p\n", obj);
	sn_rtti_t* obj_rtti = data_at(*obj);
	printf("Object has id %d, first in range is %d, last in range is %d\n", obj_rtti->type.id, ref_rtti->range.first, ref_rtti->range.last);
	int8_t ret;
	if (ref_rtti->range.last - ref_rtti->range.first == 0) {
		ret = ref_rtti->type.id == obj_rtti->type.id;
	} else {
		ret = (obj_rtti->type.id >= ref_rtti->range.first && obj_rtti->type.id <= ref_rtti->range.last);
	}
	printf("Object is class: %d\n", ret);
	return ret;
}

int8_t scalanative_is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	printf("------------is trait\n");
	return 1/0;
	/*
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
	*/
}

void* scalanative_alloc_raw(int64_t size) {
	printf("Memory allocating (%lld bytes)\n", size);
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	printf("Memory allocated to %p (%lld bytes)\n", allocated, size);

	return allocated;
}

void* scalanative_alloc_raw_atomic(int64_t size) {
	printf("Memory atomically allocating (%lld bytes)\n", size);
	void* allocated = malloc(size);
	if (allocated == NULL) {
		exit(-1);
	}

	printf("Memory atomically allocated to %p (%lld bytes)\n", allocated, size);

	return allocated;
}

void llvm_memset(int8_t* dest, int8_t val, int64_t len, int32_t align, int8_t is_volatile) {
	printf("Memsetting %p, %lld bytes\n", dest, len);
	for (int i = 0; i < len; ++i) {
		dest[i] = val;
	}
}