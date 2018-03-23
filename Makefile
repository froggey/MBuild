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
	cd Mezzano/ && $(SBCL) --dynamic-space-size 2048 --load ../build-cold-image.lisp

cold-image-vmdk: cold-image
	$(eval VM_NAME = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk '/^In use by VMs:/{print $$5}'))
	$(eval VM_UUID = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk -F ': |[()]' '/^In use by VMs: .* \(UUID:(.*)\)/{print $$4}'))
	$(eval DISK_UUID = $(shell VBoxManage showmediuminfo mezzano.vmdk |awk '/^UUID:/{print $$2}'))
	@echo "VM_NAME: $(VM_NAME)"
	@echo "VM_UUID: $(VM_UUID)"
	@echo "DISK_UUID: $(DISK_UUID)"
	-VBoxManage storagectl "$(VM_NAME)" --name IDE --remove
	-VBoxManage closemedium disk mezzano.vmdk
	rm -f mezzano.vmdk
	VBoxManage convertfromraw --format vmdk mezzano.image mezzano.vmdk

update-virutalbox: cold-image-vmdk
# This fails when the image isn't attached to any VM and there's nothing to update.
	@echo "*** Failures from VBoxManage are harmless and can be ignored. ***"
	VBoxManage storagectl "$(VM_NAME)" --name IDE --add ide --controller PIIX4
	VBoxManage storageattach "$(VM_NAME)" --storagectl IDE --port 0 --device 0 --type hdd --medium mezzano.vmdk

run-file-server: run-file-server.lisp
	cd Mezzano/file-server/ && $(SBCL) --load ../../run-file-server.lisp

qemu:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio
kvm:
	qemu-system-x86_64 -m 512 -hda mezzano.image -serial stdio -vga std -net user,hostfwd=tcp:127.0.0.1:4005-:4005 -net nic,model=virtio -enable-kvm

clean:
	rm -rf home/.cache/common-lisp/ home/.slime/ home/asdf/asdf.llf
	find Mezzano/ -name '*.llf' -type f -exec rm {} +
	rm -rf mezzano.image mezzano.map mezzano.vmdk

.PHONY: run-file-server cold-image cold-image-vmdk qemu kvm clean all update-virtualbox
