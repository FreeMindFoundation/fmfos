IMG=fmf32os.img
IMGDIR=/mnt/fmf-os/

all:
	# Free Mind Foundation OS based on MenuetOS

mount:
	mkdir -p $(IMGDIR)
	mount bin/$(IMG) $(IMGDIR)
	
kernel:
	fasm src/kernel/kernel.asm build/kernel.mnt
	cp build/kernel.mnt $(IMGDIR)

as:
	# applications here..

test:
	umount $(IMGDIR)
	qemu-system-i386 -fda bin/$(IMG)

clean:
	rm -f build/kernel.mnt
