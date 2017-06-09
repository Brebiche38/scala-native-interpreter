#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>

#include "builtins.h"

#define PATH "/Users/work/Documents/Interpreter/scala-native/sandbox/target/scala-2.11/native/bin.nbc"
#define SPILLED_SIZE (1024 * 1024)
#define PAGE_SIZE getpagesize()

extern int loop(void* data, void* oot, void* spr, size_t entry, int argc, sn_ptr_t argv) asm("loop");
extern void* opcode_offset_table[65536];

void* stack_pointer;

char* data;

int main(int argc, char** argv) {
	/** Arguments
	 1. Binary file size
	 2. Entry offset (address of main function)
	 */
	char *data_path = PATH;
	size_t data_size = strtoul(argv[1], NULL, 16);
	size_t entry_offset = strtoul(argv[2], NULL, 16);

	FILE* data_file = fopen(data_path, "r+");
	data = mmap(NULL, data_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fileno(data_file), 0);
	if (data == MAP_FAILED) {
		printf("Unable to memory map data file (%d)", errno);
		return EXIT_FAILURE;
	}

	int64_t* spilled_registers = malloc(SPILLED_SIZE);
	if (spilled_registers == NULL) {
		printf("Unable to allocate memory for spilled registers (%d)\n", errno);
	}
	mprotect(spilled_registers + SPILLED_SIZE - PAGE_SIZE, PAGE_SIZE, PROT_NONE);

	spilled_registers[0] = 0; // fake rL
	spilled_registers[1] = 0; // fake previous rReg
	spilled_registers[2] = 0; // fake size

	size_t pc = entry_offset;

	// Create custom argv
	sn_ptr_t *newargv = calloc(argc - 2, sizeof(sn_ptr_t));
	newargv[0] = argv[0] - data;
	for(int i = 3; i < argc; ++i) {
		newargv[i-2] = argv[i] - data;
	}

	return loop(data, opcode_offset_table, &spilled_registers[3], pc, 1, ((char*) newargv) - data);
}