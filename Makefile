all: p24 p24.bin p24.hex

p24: p24.c
	gcc -o p24 p24.c
	strip -s p24
p24.bin p24.hex: p24.4th
	gforth p24.4th
run: all
	./p24
core: all
	./p24 core.4th
clean:
	rm -rf p24 p24.bin p24.hex
