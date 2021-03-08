# `bme280_example`

Welcome to the `ledc_pwm_example` AtomVM application.

This example application illustrates use of the high-level LEDC PWM interface.

For this application, you will need:

* An ESP32 device, flashed with the [AtomVM](https://github.com/bettio/AtomVM) image (including the VM and core libraries), and capable of connecting via UART to your development machine;
* An LED to connect to your ESP32 device (if not already available on your development board);
* The [`esptool.py`](https://github.com/espressif/esptool) tool (for flashing);
* The [`git`](https://git-scm.com) version control tool;
* [Erlang/OTP 21](https://www.erlang.org) or higher, along with [`rebar3`](https://www.rebar3.org);
* A serial monitor program of your choice (e.g, [`minicom`](https://en.wikipedia.org/wiki/Minicom))

While the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/) and required toolchains are not required, they may make life a little easier.

## Getting Started

### Connection

Many development boards already have an LED attached to GPIO2.

However, if you are using a base ESP32, build a circuit containing an LED and 1k resistor.

    +-------------+        +-----------+
    |             |        |           |
    |        GND  +--------+           /\  LED
    |     GPIO 2  +--------+          ----
    |             |        |            |
    +-------------+        +---\/\/\/---+
         ESP32               1k resistor

> Note.  The example program assumes the LED is attached to GPIO2.  If you need to use different pins, make sure to change the example program to reflect your requirements.

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
    ===> Compiling bme280_example
    ===> AVM file written to : atomvm_lib.avm
    ===> AVM file written to : bme280_example.avm
    ===> esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/bme280_example/_build/default/lib/bme280_example.avm

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
    Found AVM partition: size: 1048576, address: 0x110000
    Starting: bme280_example.beam...
    ---
    Temperature: 18.77C, Pressure: 1012.45hPa, Humidity: 30.47%RH
    Temperature: 18.85C, Pressure: 1012.49hPa, Humidity: 30.25%RH
    Temperature: 18.93C, Pressure: 1012.42hPa, Humidity: 30.17%RH
    Temperature: 18.99C, Pressure: 1012.40hPa, Humidity: 30.43%RH
    ...
