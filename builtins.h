#ifndef __BUILTINS_H__
#define __BUILTINS_H__

#include <stdint.h>

typedef uint64_t sn_ptr_t;

typedef struct {
	int32_t id;
	sn_ptr_t name;
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
	sn_type_t type;
	int64_t size;
	sn_range_t range;
	sn_dynmap_t dynmap;
	sn_ptr_t layout;
	sn_ptr_t vtable[0];
} sn_rtti_t;

#endif /* __BUILTINS_H__ */