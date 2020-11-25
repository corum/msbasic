if [ ! -d tmp ]; then
	mkdir tmp
fi


ca65 --cpu 65C02 -t none tty.s -o tmp/tty.o &&
ld65 -t none tmp/tty.o -o tmp/tty.bin -Ln tmp/tty.lbl

cp tmp/tty.bin /mnt/c/6502/tty.bin

ca65 --cpu 65C02 -t none tty2.s -o tmp/tty2.o &&
	ld65 -t none tmp/tty2.o -o tmp/tty2.bin -Ln tmp/tty2.lbl

cp tmp/tty2.bin /mnt/c/6502/tty2.bin

