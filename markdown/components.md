# `atomvm_lib` Components

The `atomvm_lib` repository includes mostly Erlang APIs that provide added functionality to the AtomVM virtual machine.  Many of these APIs can be used on a standard AtomVM image.

However, some of these libraries make use of native-C extensions to the AtomVM virtual machine on the ESP32.  These extensions are implemented as ESP IDF SDK "components", which are collections of native C code that can be compiled and linked into the AtomVM ESP32 image.  Because the ESP IDF SDK and tool-chain does not make use of dynamic linkage, these components must be known and configured at _build_ time.

This page provides instructions for building and linking `atomvm_lib` components into an AtomVM image, so that they can be used in your applications.

## Prerequisites

* The `git` source control tool
* GNU Make
* [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/release-v3.3/index.html) and tool chain, and its pre-requisite software.
* [AtomVM](https://github.com/bettio/AtomVM) source tree, and its pre-requisite software.

> Note. These instructions assume you have downloaded the AtomVM virtual machine and have the required software needed to build the VM, including the IDF SDK and tool chain.  Instructions for building AtomVM are outside of the scope of this document.  For information about how to build AtomVM targeted for the ESP32, see the [AtomVM Build Instructions](https://doc.atomvm.org/latest/build-instructions.html#building-for-esp32).

## `atomvm_lib` Build Instructions

This section describes how to build `atomvm_lib` components into the AtomVM virtual machine.  In this section, we use `<top-level-of-atomvm-source-tree>` to refer to the top level of the AtomVM source tree.

Start by cloning the `atomvm_lib` repository into the `src/platforms/esp32/components` directory of the AtomVM source tree:

    shell$ cd <top-level-of-atomvm-source-tree>/src/platforms/esp32/components
    shell$ git clone git@github.com:atomvm/atomvm_lib.git
    ...

If you have not already built AtomVM, you can issue the `make` command from the `src/platforms/esp32` directory of the AtomVM source tree:

    shell$ cd <top-level-of-atomvm-source-tree>/src/platforms/esp32
    shell$ make
    ...

This step will compile the AtomVM sources, as well as the `atomvm_lib` sources.  However, it will not link any of the `atomvm_lib` components into the AtomVM virtual machine image.

This step should create a new AtomVM image, with the `atomvm_lib` components linked into it.

Now that you have built an AtomVM VM image containing `atomvm_lib` components, you can now flash the AtomVM image to your device.

> Note.  Flashing the AtomVM image, containing the AtomVM VM and core Erlang libraries is outside of the scope of this document.  For information about how to create an AtomVM image and flash it to an ESP32, see the [AtomVM Getting Started](https://doc.atomvm.org/latest/getting-started-guide.html#getting-started-on-the-esp32-platform) chapter of the documentation.
