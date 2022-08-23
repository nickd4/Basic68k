#!/bin/sh
dos2unix <Basic68k.L68 |grep '^0000....  [^ ]' >good
grep '^    0000.... [^ ]' Basic68k.rst >bad
paste good bad >diff
