#!/bin/sh

gcc -g -Wfatal-errors main.c opcode_offset_table.c assembly.S loop.S -o main