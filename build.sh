#!/bin/sh

set -x

TransLit pic7040.dasm TMS7000\ to\ 6809\ -\ PIC7040.txt >ssc6809.asm

lwasm --6809 ssc6809.asm -ossc6809.bin -fraw -lssc6809.lst
#lwasm --6809 -p autobranchlength -p noforwardrefmax ssc6809.asm -ossc6809.bin -fraw -lssc6809.lst

cp ssc6809.bin ~/Projects/git/tlindner/mame/roms/coco_ssc_6809/
