# ADC

The AtomVM ADC library can be used to take voltage readings from any of the 8 GPIO pins used for reading voltage signals on an ESP32 on the IDF SDK ADC1 interface, specifically for pins 32-39.

The AtomVM ADC library is only supported on the ESP32 platform.

## Build Instructions

The AtomVM ADC library is implemented as an AtomVM Nif, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this client code, you must build an AtomVM binary image.

Instructions for building and linking an `atomvm_lib` component (Nif or Port) can be found in the [Components](components.md) document.  Please read these instructions before proceeding.

The name of the ADC library Nif is `atomvm_adc`.  You should therefore edit the `component_nifs.txt` file in `src/platforms/esp32/main` so that it contains a line for the `atomvm_adc` Nif driver:

    atomvm_adc

Proceed with the instructions for building AtomVM (typically run from `src/platforms/esp32`), e.g.,

    shell$ cd .../AtomVM/src/platforms/esp32
    shell$ make
    ...
    CC .../AtomVM/src/platforms/esp32/build/atomvm_lib/nifs/atomvm_adc.o
    AR .../AtomVM/src/platforms/esp32/build/atomvm_lib/libatomvm_lib.a
    ...
    To flash all build output, run 'make flash' or:
    python /opt/esp-idf-v3.3/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -z --flash_mode dio --flash_freq 40m --flash_size detect 0x1000 .../AtomVM/src/platforms/esp32/build/bootloader/bootloader.bin 0x10000 .../AtomVM/src/platforms/esp32/build/atomvvm-esp32.bin 0x8000 .../AtomVM/src/platforms/esp32/build/partitions.bin

Once the AtomVM image is flashed to the ESP32 device, it includes the internal interfaces needed for communicating with the MQTT client library.

## Programmer's Guide

The Espressif IDF SDK and ESP32 device provides two ADC interfaces, ADC1 and ADC2.  ADC1 supports GPIO pins 32-39 for taking voltage readings, while ADC2 supports GPIO pins 0, 2, 4, 12-15, and 25-27, but with some limitations.  Currently, the `atomvm_adc` library provides integration with the ADC1 interface only; there is no support for reading voltage signals on the IDF SDK ADC2 interface, but that may be added in the future, if the need arises.

AtomVM programmers interface with the `atomvm_adc` API via the `adc` module, which provides operations for starting and stopping an Erlang process associated with a specified pin, and for taking readings on that pin.

The following code illustrates simplest use of the `atomvm_adc` API:

    %% erlang
    start() ->
        Pin = 34,
        {ok, ADC} = adc:start(Pin),
        loop(ADC).

    loop(ADC) ->
        case adc:read(ADC) of
            {ok, {Raw, MilliVolts}} ->
                io:format("Raw: ~p Voltage: ~pmV~n", [Raw, MilliVolts]);
            Error ->
                io:format("Error taking reading: ~p~n", [Error])
        end,
        timer:sleep(1000),
        loop(ADC).

The `adc:start/1` and `adc:start/2` functions will initialize a specified GPIO pin for ADC readings.  The `adc:start/2` function allows users to specify the bit width and attenuation associated with the specified pin (see below).  Both functions return a reference to the ADC pin, which is used in subsequent operations.

The `adc:read/1` and `adc:read/2` operations are used to read values from an input pin.  Readings are given in both raw values and millivolts, both integers, expressed as a pair `{Raw, MilliVolts}`.  The `adc:read/2` function can be used to specify additional options to control the behavior of the read operation (see below).

The range of raw values is determined by the configured bit width, specified as an option in the `adc:start/2` function, e.g., `adc:start(Pin, [{bit_width, bit_10}])`.  The `adc:start/1` function will supply a default bit width of `bit_12`.

The possible values are summarized in the following table:

| Bit Width | Range |
| --------- | ----- |
| `bit_9`   | 0..511 |
| `bit_10`  | 0..1023 |
| `bit_11`  | 0..2047 |
| `bit_12`  | 0..4095 |

By default, the bit resolution is `bit_12`.

> Note.  The bit width is configured globally for all ADC1 pins and cannot be adjusted individually for different pins.

The range of millivolts is determined by the attenuation setting configured for a given pin, specified as an option in the `adc:start/2` function, e.g., `adc:start(Pin, [{attenuation, db_6}])`.  The `adc:start/1` function will supply a default attenuation of `db_0`.

Attenuation settings and it corresponding voltage ranges is described in the [IDF SDK Documentation](https://docs.espressif.com/projects/esp-idf/en/v3.3.4/api-reference/peripherals/adc.html#_CPPv425adc1_config_channel_atten14adc1_channel_t11adc_atten_t) and is summarized in the following table:

| Attenuation | Voltage Range |
| ----------- | ------------- |
| `db_0`      | 100mV - 900mV  |
| `db_2_5`    | 100mV - 1250mV |
| `db_6`      | 150mV - 1750mV |
| `db_11`     | 150mV - 2450mV |

The default attenuation setting in the `atomvm_adc` library is `db_0`.

Thus, in the example above, the sample program will take readings between 100mV and 900mV on pin 34, with an approximate resolution of 800/4096 = 0.195mV.  Voltage readings greater than 900mV will flatten out and simply report the max value of approx 900mV.  To be able to resolve higher voltage, adjust the attenuation, appropriately.

> Important!  Do not supply the GPIO pin reading voltage with more than 3.6v, or you risk damaging your device.

The `adc:read/1,2` functions will sample measurements and calibrate voltage measurements, to the extent supported by the ESP32 device.  Both functions take the reference to the ADC returned from the `adc:start/1,2` functions.  The return value is a tuple containing the raw reading and voltage.

The `adc:read/2` function supports additional options, provided in a property list:

* `raw` If present, return the raw reading taken from the pin in the first element of the returned tuple (or `undefined`, if not present);
* `voltage` If present, return the converted voltage taken from the pin in the second element of the returned tuple (or `undefined`, if not present);
* `{samples, Samples}` The number of samples to take in a single reading.  The returned raw and voltage readings are averaged over the number of samples, before being returned.

> Note.  Do not specify an excessively large number of samples, as this may result in your application blocking while all samples are being read.

The `adc:read/1` function specified the following default options:

    [raw, voltage, {samples, 64}]

## API Reference

To generate Reference API documentation in HTML, issue the rebar3 target

    shell$ rebar3 edoc

from the top level of the `atomvm_adc` source tree.  Output is written to the `doc` directory.
