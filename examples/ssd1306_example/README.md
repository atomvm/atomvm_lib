# `ssd1306_example`

Welcome to the `ssd1306_example` AtomVM application.

This example application illustrates use of the `ssd1306` interface to display textual and bitmap data on an SSD1306 OLED display.

For this application, you will need:

* One SSD1306 display;
* One ESP32 device
* The [AtomVM](https://github.com/bettio/AtomVM) image (including the VM and core libraries), built with the `ssd1306` Component.  For information about building `atomvm_lib` Components, see the [`atomvm_lib` Components](../../markdown/components.md) document.

and capable of connecting via UART to your development machine;
* The [`esptool.py`](https://github.com/espressif/esptool) tool (for flashing);
* The [`git`](https://git-scm.com) version control tool;
* [Erlang/OTP 21](https://www.erlang.org) or higher, along with [`rebar3`](https://www.rebar3.org);
* A serial monitor program of your choice (e.g, [`minicom`](https://en.wikipedia.org/wiki/Minicom))

While the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/) and required tool-chains are not required, they may make life a little easier.

> Note.  These instructions assume you have flashed the AtomVM virtual machine and Erlang libraries to your ESP32 device.  For information about flashing ESP32 devices with the AtomVM virtual machine and libraries, consult the AtomVM documentation.

## Getting Started

Connect the SSD1306 display to your ESP32 device.  Use the 2-wire I2C interface to connect your display and ESP32 as follows:

| LoRa Pin | ESP32 Pin |
|----------|-----------|
| `VCC` | `+3.3v` |
| `SDA` | `GPIO 21` |
| `SCL` | `GPIO 22` |
| `GND` | `GND` |

The following diagram illustrates the connected devices:

            +-------------+        +--------------------------------+
            |         GND +--------+ GND  +---------------------+   |
            |     GPIO 21 +--------+ SDA  |      128 x 64       |   |
            |     GPIO 22 +--------+ SCL  |       display       |   |
            |       +3.3v +--------+ VCC  +---------------------+   |
            +-------------+        +--------------------------------+
                ESP32                            SSD1306


In the example program, the ESP32 will print the currently free space on the device.  It will then display a QR code (using the `qrcodegen` interface) on the screen, with more detailed information about the state of the ESP32.

### Build Instructions

We will build and flash the application for the sender and receiver in several steps.

First, let's start by compiling the source code:

    shell$ rebar3 compile
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling ssd1306_example

#### Build and Flash LoRa Receiver

Let's build and flash the LoRa receiver application on to one of your ESP32 devices.

We will first create a pruned packbeam file:

    shell$ rebar3 packbeam --prune
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling ssd1306_example
    ===> AVM file written to : ssd1306_example.avm

Next, we will flash the example program to your ESP32 device (replace `tty.usbserial.device` with the device used for your LoRa receiver):

    shell$ rebar3 esp32_flash -p /dev/tty.usbserial.device
    ...
    ===> esptool.py --chip esp32 --port /dev/tty.usbserial.device --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/ssd1306_example/_build/default/lib/ssd1306_example.avm

    esptool.py v2.1
    Connecting........_____.
    Chip is ESP32D0WDQ6 (revision (unknown 0xa))
    Uploading stub...
    Running stub...
    Stub running...
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 16384 bytes at 0x00210000 in 1.4 seconds (91.4 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting...

Connect to the device using the USB port (e.g., via `minicom`), and you should see something like:

    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f430000, size: 1048576
    Found AVM partition: size: 262144, address: 0x1d0000
    Starting: ssd1306_example.beam...
    ---
    Displaying text: [32,32,33,33,65,116,111,109,86,77,33,33,10,10,70,114,101,101,32,104,101,97,112,58,10,50,52,56,49,48,52,32,98,121,116,101,115]
    Displaying QRCode: <<254,122,71,194,9,247,63,193,68,18,146,21,74,144,110,137,168,187,241,119,203,183,92,166,22,197,170,165,219,161,108,119,224,31,146,236,20,142,147,19,118,209,7,250,170,170,170,170,170,254,0,17,10,68,114,133,0,251,152,184,63,236,135,85,16,247,129,95,92,234,197,69,198,117,59,115,47,221,68,37,147,25,86,21,21,195,52,156,194,117,149,3,86,194,233,12,88,139,142,87,154,156,222,92,188,102,205,168,101,225,9,163,251,118,163,73,169,118,120,115,69,68,249,33,241,68,56,101,110,157,147,205,125,145,253,131,32,79,99,154,150,161,237,174,98,26,32,177,98,76,7,74,196,156,195,38,23,232,64,183,49,155,9,121,217,202,121,62,156,122,29,11,129,185,86,19,117,49,212,231,161,95,163,26,200,247,255,136,175,132,94,255,79,25,172,212,122,49,81,248,173,218,162,181,183,58,128,71,187,175,24,207,132,101,127,152,187,248,251,91,253,176,17,31,163,79,250,116,10,217,185,252,44,112,204,224,220,35,200,132,188,233,202,146,20,179,123,167,132,212,175,147,83,114,19,182,210,185,211,9,82,151,10,231,169,164,24,76,9,165,25,76,223,81,105,236,25,54,129,53,78,208,114,171,92,179,184,168,123,236,144,120,26,238,163,97,92,234,51,225,170,119,10,243,46,243,198,157,144,33,56,145,230,203,232,237,210,247,145,70,217,193,113,141,188,195,128,166,153,209,197,255,218,125,69,241,108,81,159,175,226,103,3,107,189,126,42,114,248,128,114,3,209,36,120,70,255,168,73,106,154,189,235,80,75,20,84,82,245,113,251,171,130,31,240,147,79,221,215,122,247,28,33,128,66,235,248,175,158,41,90,17,5,6,117,166,156,115,28,254,213,169,22,19,113,247,0>>
    ...

If all goes according to plan, congratulations -- your SSD1306 display should be alternating between text and a QRCode every 5 seconds!  (Can you read the QRCode with your phone?)
