#!/bin/sh
srec_cat -output good -Ascii_Hex Basic68k.S68
srec_cat -output bad -Ascii_Hex Basic68k.s37
diff --unified good bad >diff
