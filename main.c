#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

#include "builtins.h"

#define PATH "/Users/work/Documents/Interpreter/scala-native/sandbox/target/scala-2.11/native/bin.nbc"

extern int loop(void* data, void* oot, void* spr, size_t entry, int argc, sn_ptr_t argv) asm("loop");
extern void* opcode_offset_table[65536];

char* data;

int main(int argc, char** argv) {
	/** Arguments
	 1. Binary file name
	 2. Binary file size
	 3. Entry offset (address of main function)
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

	printf("Data is at %p\n", data);

	int64_t* spilled_registers = malloc(8192);
	if (spilled_registers == NULL) {
		printf("Unable to allocate memory for spilled registers (%d)\n", errno);
	}

	printf("Spilled registers are at %p\n", spilled_registers);
	spilled_registers[0] = 0;

	size_t pc = entry_offset;

	printf("Entry point is %lu\n", pc);

	// Create custom argv
	char* nullstr = "";
	sn_ptr_t newargv[1];
	newargv[0] = nullstr - data; // Set memory address relative to data section

	printf("newargv is %lx, newargv[0] is %llx, nullstr is %p\n", ((char*) newargv) - data, newargv[0], nullstr);

	loop(data, opcode_offset_table, &spilled_registers[1], pc, 1, ((char*) newargv) - data);
}