#include <stdlib.h>
#include <stdint.h>

#include "builtins.h"

void llvm_memset(int8_t* dest, int8_t val, int64_t len, int32_t align, int8_t is_volatile) {
	for (int i = 0; i < len; ++i) {
		dest[i] = val;
	}
}

void llvm_memmove(char* dest, char* src, int64_t len, int32_t align, int8_t is_volatile) {
	char* tmp = scalanative_alloc(0, len);
	for (int i = 0; i < len; ++i) {
		tmp[i] = src[i];
	}
	for (int j = 0; j < len; ++j) {
		dest[j] = tmp[j];
	}
}

uint32_t llvm_ctpop(uint32_t i) {
	return __builtin_popcount(i);
}

uint32_t llvm_bswap(uint32_t i) {
	return __builtin_bswap32(i);
}

uint32_t llvm_ctlz(uint32_t i) {
	return __builtin_clz(i);
}