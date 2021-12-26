# AtomVM DHT Example Program

Welcome to the `dht_example` AtomVM application.

This example application will drive a DHT11 or DHT22 temperature and humidity sensor attached to an ESP32 device using a single data wire and print readings to the console.

The `dht_example` program illustrates use of the DHT API by taking a temperature and humidity reading every 30 seconds, and displaying the result on the console.

For this application, you will need:

* An ESP32 device, flashed with the [AtomVM](https://github.com/bettio/AtomVM) image (including the VM and core libraries), and capable of connecting via UART to your development machine;
* A DHT11 or DHT22 device, typically marketed as an integrated development board;
* The [`esptool.py`](https://github.com/espressif/esptool) tool (for flashing);
* The [`git`](https://git-scm.com) version control tool;
* [Erlang/OTP 21](https://www.erlang.org) or higher, along with [`rebar3`](https://www.rebar3.org);
* A serial monitor program of your choice (e.g, [`minicom`](https://en.wikipedia.org/wiki/Minicom))

While the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/) and required toolchains are not required, they may make life a little easier.

## Getting Started

### Connection

To run this example program, connect the positive (+) lead on the DHT device to +3.3v power on the ESP32, the negative lead (-) to a ground pin on the ESP32, and the data pin to GPIO pin 22 on the ESP32 device.

    +-----------+                           +-----------+
    |           o-------- +3.3v ------------o VCC       |
    |   DHT11   o-------- data -------------o IO22      |
    |     or    o-------- unused            |           |
    |   DHT22   o-------- gnd --------------o GND       |
    |           |                           |           |
    +-----------+                           |           |
                                            |   ESP32   |
                                            +-----------+

### Build Instructions

To build and flash this application to your ESP32 device, issue the `esp32_flash` target to the `rebar3` command, and optionally specify the device port and baud rate, if they do not match the defaults.

> Note.  For information about the `esp32_flash` target, see the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin) instructions.

    shell$ rebar3 esp32_flash -p /dev/ttyUSB0 -b 115200
    ===> Fetching atomvm_rebar3_plugin (from {git,"https://github.com/fadushin/atomvm_rebar3_plugin.git",
                                {branch,"master"}})
    ===> Fetching packbeam (from {git,"https://github.com/fadushin/atomvm_packbeam.git",
                        {branch,"master"}})
    ===> Analyzing applications...
    ===> Compiling atomvm_rebar3_plugin
    ===> Compiling packbeam
    ===> Verifying dependencies...
    ===> App atomvm_lib is a checkout dependency and cannot be locked.
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling dht_example
    ===> AVM file written to : atomvm_lib.avm
    ===> AVM file written to : dht_example.avm
    ===> esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/dht_example/_build/default/lib/dht_example.avm

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

    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f430000, size: 1048576
    Starting: dht_example.beam...
    ---
    Temperature: 21.3C  Humidity: 41.6%
    Temperature: 21.4C  Humidity: 41.4%
    Temperature: 21.3C  Humidity: 41.3%
    ...
