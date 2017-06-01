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
	printf("in field(%llx, %p, %d)\n", obj, rtti, id);
	//printf("obj at %llx, rtti at %p, id is %d\n", obj, rtti, id);
	uint64_t* offsets = data_at(rtti->layout);
	//printf("offsets at %p\n", offsets);
	printf("%llx\n", obj + offsets[id]);
	return obj + offsets[id];
}

sn_ptr_t scalanative_method_virtual(sn_ptr_t* obj, int32_t id) {
	printf("in method_virtual\n");
	if (obj == data) printf("Null pointer\n");
	//printf("Getting virtual method from obj %p at id %d, rtti is %llx\n", obj, id, *obj);
	sn_rtti_t* rtti = data_at(*obj);
	printf("meth %llx, obj %p, id %d\n", rtti->vtable[id], obj, id);
	return rtti->vtable[id];
}

sn_ptr_t scalanative_method_static(sn_ptr_t method) {
	printf("in method_static\n");
	printf("%llx\n", method);
	return method; // TODO really? deref?
}

sn_ptr_t scalanative_method_trait(sn_ptr_t* obj, sn_ptr_t* dispatchTable, int32_t methid, int32_t offset) {
	printf("in method_trait\n");
	//printf("Dispatch table is at %p\n", dispatchTable);
	//printf("Getting trait method from obj %p at offset %d (method %d)\n", obj, offset, methid);
	sn_type_t* type = data_at(*obj);
	//printf("Method ptr is at %p, id is %d\n", dispatchTable + offset + type->id, type->id);
	printf("%llx\n", *(dispatchTable + offset + type->id));

	return *(dispatchTable + offset + type->id);
}

int8_t scalanative_is_class(sn_ptr_t* obj, sn_rtti_t *ref_rtti) {
	printf("in is_class\n");
	if (obj == data) {
		//printf("Null pointer\n");
		return 0;
	}
	//printf("Object is at %p\n", obj);
	sn_rtti_t* obj_rtti = data_at(*obj);
	//printf("Object has id %d, first in range is %d, last in range is %d\n", obj_rtti->type.id, ref_rtti->range.first, ref_rtti->range.last);
	int8_t ret;
	printf("first %d, last %d\n", ref_rtti->range.last, ref_rtti->range.first);
	if (ref_rtti->range.last - ref_rtti->range.first == 0) {
		ret = ref_rtti->type.id == obj_rtti->type.id;
	} else {
		ret = (obj_rtti->type.id >= ref_rtti->range.first && obj_rtti->type.id <= ref_rtti->range.last);
	}
	printf("%d\n", ret);
	return ret;
}

int8_t scalanative_is_trait(sn_type_t** obj, int8_t** classHasTrait, int32_t traitId) {
	printf("!!!!!!!!!!!!!in is_trait\n");
	return 1/0;
	/*
	int32_t classId = (*obj)->id;
	return classHasTrait[classId][traitId];
	*/
}

void llvm_memset(int8_t* dest, int8_t val, int64_t len, int32_t align, int8_t is_volatile) {
	printf("in memset\n");
	//printf("Memsetting %p, %lld bytes\n", dest, len);
	for (int i = 0; i < len; ++i) {
		dest[i] = val;
	}
}

uint32_t llvm_ctpop(uint32_t i) {
	printf("in ctpop\n");
	return __builtin_popcount(i);
	/*
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    */
}

uint32_t llvm_bswap(uint32_t i) {
	printf("in bswap\n");
	return __builtin_bswap32(i);
}

uint32_t llvm_ctlz(uint32_t i) {
	printf("in ctlz\n");
	return __builtin_clz(i);
}

void elem(sn_ptr_t ptr, sn_ptr_t base, uint64_t idx, uint64_t size) {
	printf("elem: ptr %llx, base %llx, id %lld, size %lld\n", ptr, base, idx, size);
}

int8_t scalanative_platform_is_windows() {
#ifdef _WIN32
    return 1;
#else
    return 0;
#endif
}