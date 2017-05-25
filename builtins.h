#ifndef __BUILTINS_H__
#define __BUILTINS_H__

#include <stdint.h>

typedef uint64_t sn_ptr_t;

typedef struct {
	int32_t id;
	sn_ptr_t name; // string
	int8_t kind;
} sn_type_t;

typedef struct {
	int32_t first;
	int32_t last;		
} sn_range_t;

typedef struct {
	int32_t size;
	sn_ptr_t intkeys;
	sn_ptr_t keys;
	sn_ptr_t values;
} sn_dynmap_t;

typedef struct {
	sn_ptr_t type; // sn_type_t
	int64_t size;
	sn_ptr_t range; // sn_range_t
	sn_ptr_t dynmap; // sn_dynmap_t
	sn_ptr_t layout; // array[int64_t]
	sn_ptr_t vtable; // array[sn_ptr_t]
} sn_rtti_t;

#endif /* __BUILTINS_H__ */