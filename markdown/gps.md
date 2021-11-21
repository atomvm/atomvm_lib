# AtomVM GPS Driver

The AtomVM GPS library can be used to take readings from common GSP sensors attached to your ESP32 device.

At a high level, the following functionality is supported:

* Lifecycle management, via common `start/1` and `stop/1` semantics;
* Connectivity to a GPS sensor over the UART protocol, supporting NMEA message parsing, including configuration of UART parameters;
* Asynchronous reading GPS measurements from the GPS sensor;
* Filtering readings to reduce overhead of message passing for unwanted fields.

The AtomVM GPS library is only supported on the ESP32 platform.

## Build Instructions

The AtomVM GPS library is implemented as an AtomVM Port, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this client code, you must build an AtomVM binary image.

Instructions for building and linking an `atomvm_lib` component (Nif or Port) can be found in the (Components)[components.md] document.  Please read these instructions before proceeding.

The name of the AtomVM GPS library port is `atomvm_gps`.  You should therefore edit the `component_ports.txt` file in `src/platforms/esp32/main` so that it contains a line for the AtomVM MQTT client port:

    atomvm_gps

Proceed with the instructions for building AtomVM (typically run from `src/platforms/esp32`), e.g.,

    shell$ cd .../AtomVM/src/platforms/esp32
    shell$ make
    ...
    CC .../AtomVM/src/platforms/esp32/build/atomvm_lib/ports/atomvm_atomvm_gps.o
    AR .../AtomVM/src/platforms/esp32/build/atomvm_lib/libatomvm_lib.a
    ...
    To flash all build output, run 'make flash' or:
    python /opt/esp-idf-v3.3/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -z --flash_mode dio --flash_freq 40m --flash_size detect 0x1000 .../AtomVM/src/platforms/esp32/build/bootloader/bootloader.bin 0x10000 .../AtomVM/src/platforms/esp32/build/atomvvm-esp32.bin 0x8000 .../AtomVM/src/platforms/esp32/build/partitions.bin

Once the AtomVM image is flashed to the ESP32 device, it includes the internal interfaces needed for communicating with the MQTT client library.

## Programmer's Guide

The library assumes that GPS sensors are connected to the ESP32 via the UART interface, and that the GPS sensor supports the [NMEA 0183](https://en.wikipedia.org/wiki/NMEA_0183) protocol.  Examples include boards built on the [NEO-6](https://datasheetspdf.com/pdf-file/866235/u-blox/NEO-6M/1) chipset.  The GPS sensor only needs to be connected to the RX channel on the UART interface, as messages are only sent from the GPS sensor to the ESP32.

More detailed information about the UART interface is available in the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/v3.3.5/api-reference/peripherals/uart.html) documentation.

Once connected and initialized, the GPS sensor should send readings to your application, in a manner described below.  GPS readings include date and time, latitude, longitude, altitude, and speed, among other readings.  More details are provided below.


### Lifecycle

The gps library functionality is encapsulated in an Erlang process which you can start via the `start/1` function:

    %% erlang
    Config = #{
        ...
    },
    {ok, GPS} = gps:start(Config).

The returned reference can be used for subsequent operations.

The input parameter to the `start/1` function is an Erlang `map`, containing configuration values under well-defined atomic keys that are used to initialize the GPS sensor and interface to the device.

The following table summarizes the keys that can be specified in this `map`:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `uart_port` | `uart_0 \| uart_1 \| uart_2`| `uart_1` | One of three possible UART hardware interfaces on the ESP32 |
| `rx_pin` | integer | | Optional GPIO pin to map RX signal |
| `baud_rate` | integer | 9600 | UART BAUD rate |
| `data_bits` | `data_bits_5 \| data_bits_6 \| data_bits_7 \| data_bits_8` | `data_bits_8` | UART data bits |
| `stop_bits` | `stop_bits_1 \| stop_bits_1_5 \| stop_bits_2` | `stop_bits_1` | UART stop bits |
| `parity` | `disable \| odd \| even` | `disable` | UART parity |
| `event_queue_size` | integer | 16 | UART event queue size |
| `gps_reading_filter` | `[datetime \| latitude \| longitude \| altitude \| speed \| sats_in_use \| fix \| fix_mode \| valid \| sats_in_view]` | | A list of keys from the GPS reading map to include when sending GPS readings to the application.  By not setting this key in configuration, all available keys will be sent in a GPS reading. |
| `gps_reading_handler` | `fun(gps(), gps_reading() -> any())` | | Callback function which will be invoked when a GPS reading is delivered.  (See [Receiving GPS Readings](#Receiving_GPS_Readings) below.) |

If the `rx_pin` is not specified, then the default RX pin will be used, depending on the specified `uart_port`.

| `uart_port` | Default RX pin |
|-------------|----------------|
| `uart_0` | GPIO3 |
| `uart_1` | GPIO9 |
| `uart_2` | GPIO16 |

Example configuration:

    %% erlang
    Config = #{
        uart_port => uart_1,
        rx_pin => 2,
        gps_reading_handler => fun handle_gps_reading/2
    }

To stop an GPS instance, use the `stop/1` function, supplying a reference to the GPS instance returned from `start/1`:

    %% erlang
    ok = gps:stop(GPS).

### Receiving GPS Readings

When the GPS sensor delivers a reading to the application, and when a callback function is specified in the GPS configuration using the `gps_reading_handler` key, then the specified function will be called, with the GPS reading supplied as a parameter.  For example,

    %% erlang
    handle_gps_reading(GPS, GPSReading) ->
        io:format("Received GPS reading from ~p: ~p~n", [GPSReading, GPS]),
        ...

> Note.  The `atomvm_gps` library provides no guarantees about the execution context in which this function is invoked.  Applications should make no assumptions about any information available (e.g., entries in the process dictionary) in the context in which this function is executed.

A GPS reading is a `map` that includes the following keys and values:

| Key | Type | Value |
|-----|------|-------|
| `datetime` | `{{year(), month(), day()}, {hour(), minute(), second()}}` | Date and time, expressed as a pair containing the date and time.  The date is a triple containing the year, month, and day (of the month), all expressed as integers.  The time is a triple containing the hour, minute, and second, all expressed as integers. |
| `latitude` | `{integer(), non_neg_integer()}` | Latitude in degrees, expressed as a pair containing an integral and fractional part (precision to 4 decimal places). |
| `longitude` | `{integer(), non_neg_integer()}` | Longitude in degrees, expressed as a pair containing an integral and fractional part (precision to 4 decimal places. |
| `altitude` | `{integer(), non_neg_integer()}` | Altitude in meters above or below sea level, expressed as a pair containing an integral and fractional part (precision to 4 decimal places. |
| `speed` | `{non_neg_integer(), non_neg_integer()}` | Speed in meters per second, expressed as a pair containing an integral and fractional part (precision to 4 decimal places. |
| `sats_in_use`| `[non_neg_integer()]` | List of satellite IDs in use. |
| `fix` | `invalid \| gps \| dgps` | Invalid, GPS, or differential-GPS |
| `fix_mode` | `invalid \| mode_2d \| mode_3d` | |
| `valid` | `true \| false` | |
| `sats_in_view`| `[satellite()]` | List of satellites in view.  Each `satellite()` is a `map` containing the satellite id (`num`), elevation in degrees (`elevation`), azimuth in degrees (`azimuth`), and signal-to-noise ratio in db (`snr`). |

C.f., [FreeNMEA documentation](http://freenmea.net/docs)

If you would only like to receive a subset of the entries in this map in your handler, you can use the `gps_reading_filter` key in the configuration map passed to the `gps:start/1` function to enumerate the entries you would like to see.  Use the keys described above to _include_ the desired fields.  If no filter is specified, then _all_ fields will be delivered.  If an empty list is specified, then _no_ fields will be delivered.

For example,

    %% erlang
    Config = #{
        ...
        gps_reading_filter => [datetime, latitude, longitude, altitude, speed, valid],
        ...
    }

## API Reference

To generate Reference API documentation in HTML, issue the `rebar3` target

    shell$ rebar3 edoc

from the top level of the `atomvm_gps` source tree.  Output is written to the `doc` directory.
