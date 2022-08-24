#!/bin/sh
dos2unix <Basic68k.L68 |grep '^0000....[ =] [0-9A-F]\|ds\.' >good
grep '^    0000.... [0-9A-F]\|\.blk' Basic68k.rst >bad
paste good bad >diff
