# `bh1750_example`

Welcome to the `bh1750_example` AtomVM application.

This example application will drive a BH1750 luminosity sensor attached to an ESP32 device using the 2-wire I2C interface and print luminosity values in [lux](https://en.wikipedia.org/wiki/Lux) to the console.

For this application, you will need:

* An ESP32 device, flashed with the [AtomVM](https://github.com/bettio/AtomVM) image (including the VM and core libraries), and capable of connecting via UART to your development machine;
* A BH1750 device, typically marketed as an integrated development board;
* The [`esptool.py`](https://github.com/espressif/esptool) tool (for flashing);
* The [`git`](https://git-scm.com) version control tool;
* [Erlang/OTP 21](https://www.erlang.org) or higher, along with [`rebar3`](https://www.rebar3.org);
* A serial monitor program of your choice (e.g, [`minicom`](https://en.wikipedia.org/wiki/Minicom))

While the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/) and required toolchains are not required, they may make life a little easier.

## Getting Started

### Connection

Use of the BH1750 will typically require connecting some kind of development board, which includes the BH1750 integrated circuit, along with additional passive components, to you ESP32 device.

Connect the VCC and GND pins to a power source and ground (e.g., on your ESP32).

Connect the SDA and SCL pins on two selected GPIO pins on your ESP32 device (e.g., pin 21 and 22).

    +-----------+                         +-------------+
    |       VCC +-------------------------+ +3.3v       |
    |       GND +-------------------------+ GND         |
    |       SDA +-------------------------+ GPIO 21     |
    |       SCL +-------------------------+ GPIO 22     |
    +-----------+                         +-------------+
       BH1750                                  ESP32

> Note.  The example program assumes the SDA pin is connected to the ESP32 GPIO pin 21 and the SCL pin is connected to the ESP32 GPIO pin 22.  If you need to use different pins, make sure to change the example program to reflect your requirements.

### Build Instructions

To build and flash this application to your ESP32 device, issue the `esp32_flash` target to the `rebar3` command, and optionally specify the device port and baud rate, if they do not match the defaults.

> Note.  For information about the `esp32_flash` target, see the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin) instructions.

    shell$ rebar3 esp32_flash -p /dev/tty.usbserial-01940306
    ===> Verifying dependencies...
    ===> App atomvm_lib is a checkout dependency and cannot be locked.
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling bh1750_example
    ===> AVM file written to : bh1750_example.avm
    ===> esptool.py --chip esp32 --port /dev/tty.usbserial-01940306 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/bh1750_example/_build/default/lib/bh1750_example.avm

    esptool.py v2.1
    Connecting........_
    Chip is ESP32D0WDQ6 (revision 1)
    Uploading stub...
    Running stub...
    Stub running...
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 16384 bytes at 0x00210000 in 1.4 seconds (91.7 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting...

Connect to the device using the USB port (e.g., via `minicom`), and you should see something like:

    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f430000, size: 1048576
    Found AVM partition: size: 1048576, address: 0x110000
    Starting: bh1750_example.beam...
    ---
    Luminosity: 153.22lx
    Luminosity: 106.66lx
    Luminosity: 107.50lx
    Luminosity: 107.50lx
    Luminosity: 107.50lx
    Luminosity: 106.66lx
    Luminosity: 107.50lx
    Luminosity: 107.50lx
    Luminosity: 90.0lx
    Luminosity: 74.16lx
    ...
