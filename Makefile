Basic68k.s37: Basic68k.rel
	./aslink -nsu -a text=0x400 -a data=0x40000 $<

Basic68k.rel: Basic68k.asm
	./as68k -lo+$@ $<

clean:
	rm -f *.lst *.hlr *.rel *.rst *.s37
