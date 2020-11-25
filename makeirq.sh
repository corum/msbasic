if [ ! -d tmp ]; then
	mkdir tmp
fi


ca65 --cpu 65C02 -t none irq.s -o tmp/irq.o &&
ld65 -t none tmp/irq.o -o tmp/irq.bin -Ln tmp/irq.lbl

cp tmp/irq.bin /mnt/c/6502/irq.bin

ca65 --cpu 65C02 -t none setupvia.s -o tmp/setupvia.o &&
	ld65 -t none tmp/setupvia.o -o tmp/setupvia.bin -Ln tmp/setupvia.lbl

cp tmp/setupvia.bin /mnt/c/6502/setupvia.bin
