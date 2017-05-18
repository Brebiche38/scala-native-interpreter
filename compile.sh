#!/bin/sh

gcc -g -Wfatal-errors main.c builtins.c opcode_offset_table.c assembly.S loop.S -o main