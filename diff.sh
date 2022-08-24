#!/bin/sh
srec_cat -output - -Binary Basic68k.S68 |xxd >good
srec_cat -output - -Binary Basic68k.s37 |xxd >bad
diff --unified good bad >diff
