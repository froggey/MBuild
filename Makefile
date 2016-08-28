# This must be the SBCL executable to use
SBCL := sbcl
# This must be the IP address of the file-server.
# Note! Addresses on 10.0.2/24 networks are not supported, as this conflicts
# with the network provided by qemu and VirtualBox.
FILE_SERVER_IP := 192.168.0.555

# Report an error if this hasn't been configured.
ifeq ($(FILE_SERVER_IP),192.168.0.555)
# Unless no target was specified.
ifneq ($(MAKECMDGOALS),)
$(error FILE_SERVER_IP has not been configured in the Makefile)
endif
endif

all:
	@echo "Quick start:"
	@echo " 0. Set SBCL path and FILE_SERVER_IP in Makefile."
	@echo "    Run git submodule update --init"
	@echo " 1. Run make cold-image-vmdk"
	@echo " 2. In a seperate terminal, run make run-file-server"
	@echo "    The file server needs to run while the VM is running."
	@echo " 3. Point VirtualBox at mezzano.vmdk and start the VM."

cold-image: build-cold-image.lisp
	@echo File server address: $(FILE_SERVER_IP)
	@echo Source path: $(CURDIR)/Mezzano/
	@echo Home directory path: $(CURDIR)/home/
	echo "(in-package :sys.int)" > Mezzano/config.lisp
	echo "(defparameter *file-server-host-ip* \"$(FILE_SERVER_IP)\")" >> Mezzano/config.lisp
	echo "(defparameter *home-directory-path* \"$(CURDIR)/home/\")" >> Mezzano/config.lisp
	echo "(defparameter *mezzano-source-path* \"$(CURDIR)/Mezzano/\")" >> Mezzano/config.lisp
	cd Mezzano/ && $(SBCL) --load ../build-cold-image.lisp

cold-image-vmdk: cold-image
	rm -f mezzano.vmdk
	VBoxManage convertfromraw --format vmdk mezzano.image mezzano.vmdk

run-file-server: run-file-server.lisp
	cd Mezzano/file-server/ && $(SBCL) --load ../../run-file-server.lisp

qemu:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio
kvm:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio -enable-kvm

clean:
	rm -rf home/.cache/common-lisp/ home/.slime/ home/asdf/asdf.llf
	rm -rf Mezzano/*.llf Mezzano/*/*.llf
	rm -rf mezzano.image mezzano.map mezzano.vmdk

.PHONY: run-file-server cold-image cold-image-vmdk qemu kvm clean all
