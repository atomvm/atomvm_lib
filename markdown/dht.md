# DHT

The AtomVM DHT library can be used to drive DHT11 and DHT12 temperature and humidity sensors that can be attached to ESP32 devices.

The AtomVM DHT library is only supported on the ESP32 platform.

## Build Instructions

The AtomVM DHT library is implemented as an AtomVM Nif, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this client code, you must build an AtomVM binary image.

Instructions for building and linking an `atomvm_lib` component (Nif or Port) can be found in the [Components](components.md) document.  Please read these instructions before proceeding.

The name of the DHT library Nif is `atomvm_dht`.  You should therefore edit the `component_nifs.txt` file in `src/platforms/esp32/main` so that it contains a line for the `atomvm_dht` Nif driver:

    atomvm_dht

Proceed with the instructions for building AtomVM (typically run from `src/platforms/esp32`), e.g.,

    shell$ cd .../AtomVM/src/platforms/esp32
    shell$ make
    ...
    CC .../AtomVM/src/platforms/esp32/build/atomvm_lib/nifs/atomvm_dht.o
    AR .../AtomVM/src/platforms/esp32/build/atomvm_lib/libatomvm_lib.a
    ...
    To flash all build output, run 'make flash' or:
    python /opt/esp-idf-v3.3/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -z --flash_mode dio --flash_freq 40m --flash_size detect 0x1000 .../AtomVM/src/platforms/esp32/build/bootloader/bootloader.bin 0x10000 .../AtomVM/src/platforms/esp32/build/atomvvm-esp32.bin 0x8000 .../AtomVM/src/platforms/esp32/build/partitions.bin

Once the AtomVM image is flashed to the ESP32 device, it includes the internal interfaces needed for communicating with the AtomVM DHT library.

## Programmer's Guide

The DHT API can be used to drive common DHT11 and DHT22 temperature and humidity sensors.

DHT22 devices are reported to have higher resolution and range in both temperature and humidity readings.

Temperature and humidity readings can be taken at intervals not less that 1 second apart for DHT11 devices, and 2 seconds apart for DHT22 devices.  The DHT API will ensure that any one DHT instance will not read at less than the recommended interval.

### Lifecycle

To start the DHT driver, use the `dht:start/1` function.  Pass in a configuration map, which may contain the following entries

| Key | Value | Default | Required | Description |
|-----|-------|---------|----------|-------------|
| `pin` | `integer()` | none | yes | The data pin to which the DHT11 or DHT22 is connected. |
| `device` | `dht_11 \| dht_12` | `dht_11` | no | The device type (DHT11 or DHT12). |

For example:

    %% erlang
    Config = #{
        pin => 22,
        device => dht_11
    },
    {ok, DHT} = dht:start(Config),
    ...

Stop the DHT driver via the `dht:stop/1` function.  Supply the reference to the DHT instance returned from `dht:start/1`:

    %% erlang
    ok = dht:stop(DHT)

### Taking Readings

Take readings using the `dht:take_reading/1` function.  Supply a reference to the DHT instance returned from `dht:start/1`.

A reading is expressed as pair containing the temperature (in degrees celcius) and relative humidity (expressed as a percentage).  The temperature and humidity readings are each pairs of integers, representing the whole and fractional part (to a precision of one digit).

For example,

    %% erlang
    case dht:take_reading(DHT) of
        {ok, Reading} ->
            {{Temp, TempFractional}, {Hum, HumFractional}} = Reading,
            io:format("Temperature: ~p.~pC  Humidity: ~p.~p%~n", [Temp, TempFractional, Hum, HumFractional]);
        Error ->
            io:format("Error taking reading: ~p~n", [Error])
    end,
    ...
