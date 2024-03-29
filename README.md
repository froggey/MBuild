This repo contains the build environment used to build Mezzano.  As
part of this procedure, Mezzano itself is cloned as a sub-project to
this project.  Therefore, an independent clone of Mezzano is
unnecessary.


## Prerequisites

A recent 64-bit version of SBCL with Unicode support is required.
SBCL may be obtained at [www.sbcl.org](https://www.sbcl.org)

Versions `1.2.4`, `1.2.10`, `1.3.1`, `1.3.2`, `1.3.5`, and `1.4.5` are known to work.
Newer versions should work, but have not been tested.

The following systems should be installed using Quicklisp:
```
Alexandria
Iterate
Nibbles
CL-PPCRE
CL-FAD
closer-mop
trivial-gray-streams
```

To install the packages with Quicklisp, install SBCL and Quicklisp.  Then at a shell prompt, run:
```shell
sbcl --script /path/to/quicklisp.lisp
```
(on Debian and Ubuntu based Linux systems, if you install Quicklisp through the package manager it's in `sbcl --script /usr/share/cl-quicklisp/quicklisp.lisp`)

In SBCL type each of the following commands:
```common-lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(ql:quickload '(alexandria iterate nibbles cl-fad cl-ppcre closer-mop trivial-gray-streams))
(quit)
```

Additionally, you will need VirtualBox to create the vmdk file and either QEMU or VirtualBox to run it.


## Quick Start

0. Run `git submodule update --init`
   Set `SBCL` and `FILE_SERVER_IP` in the top-level Makefile.
   `SBCL` must point at the SBCL binary, and `FILE_SERVER_IP` to the IP address
   of the local machine.

   Note! Addresses on `10.0.2.0/24` networks are not supported (such as `10.0.2.5`), as
   this conflicts with the network provided by QEMU and VirtualBox.
   IPv6 addresses are not supported.

1. Run `make cold-image-vmdk`
   This will build a cold image suitable for use with VirtualBox, and
   should only take a few minutes.

2. Run `make run-file-server`
   The file-server must be running during step 3.

3. Configure a VirtualBox VM to point at the new `mezzano.vmdk` image and run it.
```
   VM settings:
   General->Type: Other
            Version: Other/Unknown (64-bit)
   System->Motherboard->Base Memory: 2GB
   System->Acceleration->Enable VT-x/AMV-V: enabled
                         Enable Nested Paging: enabled
   Storage->IDE controller with mezzano.vmdk attached on the Primary Master hard disk.
   Network->Adapter 1: enabled
            Attached to: NAT
            Adapter Type (advanced): virtio-net
   Serial Ports->Port 1: enabled
            Port Mode: Raw file
            Path: /some/file
   Audio->Audio Controller: Intel HD Audio
```

4. Wait. The rest of the system is being compiled.
   Eventually a REPL and the desktop will appear.

5. Finish up.
   Once the desktop has appeared and the system has finished working (no blue
   run light) then (snapshot-and-exit) can be run in the initial REPL to
   finalize the state of the system and close the initial REPL.


Due to the use of submodules you need to run `git submodule update --init` after pulling to update to the latest version.


## Using QEMU/KVM instead of VirtualBox

After building a cold image and starting the file-server as
described by the Quick Start section above run
`make qemu` or `make kvm` to run QEMU.
Using KVM is recommended as it is much faster than QEMU.

Note that the first time you run Mezzano, a lot of the code is compiled and boot time can take ten minutes or more on slow machines.  Subsequent boots are much faster, typically under two minutes.
