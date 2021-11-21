# AtomVM ADC Example Program

The `adc_example` program illustrates use of the ADC API by reading the voltage signal on pin 34 once every second, and displaying the result on the console.

> Note.  Building and flashing the `adc_example` program requires installation of the [`rebar3`](https://www.rebar3.org) Erlang build tool.

To run this example program, you will need to connect a power source to pin 34 on your ESP32 device, such as through a power supply (e.g., DPH5005), and provide it with a voltage between 0 and 1 volts.  Connect the negative lead on your power supply to the ground pin on your ESP32.

                                                +-----------------------+
    +-----------+ gnd                       neg |   +--------------+    |
    |           +-------------------------------+   |       0.50V  |    |
    |   ESP32   |                               |   |              |    |
    |           | 34                        pos |   +--------------+    |
    |           +-------------------------------+                       |
    |           |                               +-----------------------+
    |           |                               power supply
    |           |
    +-----------+

> Important!  Do not supply the GPIO pin reading voltage with more than 3.6v, or you risk damaging your device.

Build the example program and flash to your device:

    shell$ cd .../AtomVM/src/platforms/esp32/components/atomvm_dht/examples/adc_example
    shell$ rebar3 esp32_flash -p /dev/ttyUSB0

> Note.  This build step makes use of the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin).  See the `README.md` for information about parameters for setting the serial port and baud rate for your platform.

Attach to the console usin gthe `monitor` Make target in the AtomVM ESP32 build:

    shell$ cd .../AtomVM/src/platform/esp32
    shell$ make monitor
    Toolchain path: /work/opt/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc
    WARNING: Toolchain version is not supported: crosstool-ng-1.22.0-95-ge082013a
    ...
    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f420000, size: 1048576
    I (243) atomvm_adc: eFuse Two Point: NOT supported
    I (243) atomvm_adc: eFuse Vref: Supported
    Found AVM partition: size: 1048576, address: 0x110000
    Starting: adc_example.beam...
    ---
    Raw: 6 Voltage: 76mV
    Raw: 0 Voltage: 75mV
    Raw: 0 Voltage: 75mV

By adjusting input voltage on your power supply, you should see changes in the reported readings:

    ...
    Raw: 1731 Voltage: 488mV
    Raw: 1746 Voltage: 491mV
    Raw: 1737 Voltage: 489mV
    Raw: 1728 Voltage: 487mV
    Raw: 1737 Voltage: 489mV
    Raw: 1732 Voltage: 488mV
    Raw: 1739 Voltage: 489mV
    ...
    Raw: 3017 Voltage: 794mV
    Raw: 3010 Voltage: 792mV
    Raw: 3016 Voltage: 794mV
    Raw: 3019 Voltage: 795mV
    Raw: 3003 Voltage: 791mV

Note that input values above
