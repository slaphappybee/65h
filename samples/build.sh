#!/bin/bash

echo Building sample "barebones"
65a barebones.asm
65l barebones.asm.prg

echo Building sample "stripes"
65a stripes.asm
65l stripes.asm.prg

echo Building sample "text"
65a text.asm
65e text-files/font.bmp
65l text.asm.prg text-files/font.bmp.chr
