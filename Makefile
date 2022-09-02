Basic68k.s37: Basic68k.rel
	./aslink -p0 -a text=0x400 -a data=0x40000 -s -u $<

Basic68k.rel: Basic68k.asm
	./as68k -l -o $@ $<

clean:
	rm -f *.lst *.hlr *.rel *.rst *.s37
