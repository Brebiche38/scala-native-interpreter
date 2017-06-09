#!/bin/sh

gcc -g -Wfatal-errors src/*.c src/builtins/*.c src/*.S -Iinc/ -o main