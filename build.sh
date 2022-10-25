#!/bin/bash

rm src/main.o
rm bin/cartridge.nes

ca65 src/main.asm -D INC_MUSIC -D NES
ld65 -C src/nes.cfg -o bin/cartridge.nes src/main.o -m src/music_nes_map.txt
# cl65 -t nes -o bin/cartridge.nes -l src.main.list src/main.asm

fceux bin/cartridge.nes
