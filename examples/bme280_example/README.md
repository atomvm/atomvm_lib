# `bme280_example`

Welcome to the `bme280_example` AtomVM application.

To build and flash this application to your ESP32 device, issue the `esp32_flash` target

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
    ===> esptool.py --chip esp32 --port /dev/ttyUSB0 --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 /Users/fadushin/work/src/github/fadushin/atomvm_lib/examples/bme280_example/_build/default/lib/bme280_example.avm

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
