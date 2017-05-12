#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

#define PATH "/Users/work/Documents/Interpreter/scala-native/sandbox/target/scala-2.11/native/bin.nbc"

extern int loop(void* data, void* oot, void* spr, size_t entry) asm("loop");
extern void* opcode_offset_table[65536];

uint8_t part(uint16_t opcode, size_t byte) {
	return (opcode & (0xf << 4*(3-byte))) >> 4*(3-byte);
}

int main(int argc, char** argv) {
	/** Arguments
	 1. Binary file name
	 2. Binary file size
	 3. Entry offset (address of main function)
	 */
	char *data_path = PATH;
	size_t data_size = atoi(argv[1]);
	size_t entry_offset = atoi(argv[2]);

	FILE* data_file = fopen(data_path, "r");
	char* data = mmap(NULL, data_size, PROT_READ, MAP_PRIVATE, fileno(data_file), 0);
	if (data == MAP_FAILED) {
		printf("Unable to memory map data file (%d)", errno);
		return EXIT_FAILURE;
	}

	printf("Data is at %p\n", data);

	void* spilled_registers = malloc(4096);
	if (spilled_registers == NULL) {
		printf("Unable to allocate memory for spilled registers (%d)\n", errno);
	}

	printf("Spilled registers are at %p\n", spilled_registers);

	size_t pc = entry_offset;

	printf("Entry point is %lu\n", pc);

	loop(data, opcode_offset_table, spilled_registers, pc);
}