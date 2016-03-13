SBCL := sbcl
FILE_SERVER_IP := 192.168.0.123

all:
	@echo "Steps to build:"
	@echo " 0. Set SBCL path and FILE_SERVER_IP in Makefile."
	@echo "    Run git submodule update --init"
	@echo " 1. Run build-cold-image."
	@echo " 2. In a seperate terminal, run make run-file-server."
	@echo "    The file server needs to run while qemu is running."
	@echo " 3. Run make qemu or make kvm."
	@echo "    Use make kvm when possible, it is much faster."

build-cold-image: build-cold-image.lisp
	echo "(in-package :sys.int)" > Mezzano/config.lisp
	echo "(defparameter *file-server-host-ip* \"$(FILE_SERVER_IP)\")" >> Mezzano/config.lisp
	echo "(defparameter *home-directory-path* \"$(CURDIR)/home/\")" >> Mezzano/config.lisp
	echo "(defparameter *mezzano-source-path* \"$(CURDIR)/Mezzano/\")" >> Mezzano/config.lisp
	cd Mezzano/ && $(SBCL) --load ../build-cold-image.lisp

build-cold-image-vmdk: build-cold-image
	rm -f mezzano.vmdk
	VBoxManage convertfromraw --format vmdk mezzano.image mezzano.vmdk

run-file-server: run-file-server.lisp
	cd Mezzano/file-server/ && $(SBCL) --load ../../run-file-server.lisp

qemu:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user -net nic,model=virtio
kvm:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user -net nic,model=virtio -enable-kvm

clean:
	rm -rf home/.cache/common-lisp/ home/.slime/ home/asdf/asdf.llf
	rm -rf Mezzano/*.llf Mezzano/*/*.llf
	rm -rf mezzano.image mezzano.map mezzano.vmdk

.PHONY: run-file-server build-cold-image build-cold-image-vmdk qemu kvm clean all
