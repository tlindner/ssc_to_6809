This is an attempt to transcode a TMS7000 program to m6809.

The TMS7000 program is 4k and resides in a TMS7040 microcontroller inside a Radio Shack Color Computer Speech / Sound cartridge.

The program handles commands from the Color Computer and subsequently controls the internal PSG, Speech chip, and 2k of RAM.

Currently I am targeting an emulated device that can be found in one of my MAME branches.

https://github.com/tlindner/mame/tree/coco_ssc6809-2

But the ultimate goal is to create a new cartridge.

The process starts with a disassembly in the file pic7040.dasm. It is lightly modified.

It is run thru another one of my projects (https://github.com/tlindner/TransLit) with the data file TMS7000 to 6809 - PIC7040.txt.

That file contain dozens of regular expressions to convert TMS7000 instructions into 6809 instructions.

It produces ssc6809.asm that is assembled by lwasm. http://www.lwtools.ca

The file 'build.sh' has all these steps baked into a script.

November 6, 2020: It is currently not working.

tim lindner
