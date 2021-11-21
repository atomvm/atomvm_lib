# AtomVM GPS Example Program

The `gps_example` program illustrates integration with a common GPS sensor to take GPS readings from satellites in the sky.  Results are displayed on the console.

> Note.  Depending on the device in use, taking rGPS eadings from satellites will generally require an unobstructed view of the sky (i.e., outdoors).  Getting a fix from satellites may require considerable time (upwards of 10 minutest), depending on atmospheric conditions, RF interference, and so forth.

To run this example program, you will need to connect your ESP32 device to a GSP module.  This example program assumes a development board based on the [NEO-6](https://datasheetspdf.com/pdf-file/866235/u-blox/NEO-6M/1) chipset, or equivalent.

Connect the negative lead on your GPS sensor to the ground pin on your ESP32, the VCC on your GPS sensor to a +5v power source, and the TX lead on the GPS sensor to GPIO2 on your ESP32.

                                      +-----------+
    +-----------+ gnd             neg |           |
    |           +---------------------+           +
    |   ESP32   |                     |           |
    |           | 2                TX |           |
    |           +---------------------+           +
    |           |                     |           |
    |           | +5v             pos |           |
    |           +---------------------+           +
    |           |                     +-----------+
    |           |                       GPS Sensor
    |           |
    |           |
    |           |
    +-----------+

The application program will start the GPS sensor, and then print a subset of the data from a GPS reading to the console, as the device sends data to the ESP32.

> Note.  Building and flashing the `gps_example` program requires installation of the [`rebar3`](https://www.rebar3.org) Erlang build tool.

Build the example program and flash to your device:

    shell$ cd .../AtomVM/src/platforms/esp32/components/atomvm_gps/examples/gps_example
    shell$ rebar3 esp32_flash -p /dev/ttyUSB0

> Note.  This build step makes use of the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin).  See the `README.md` for information about parameters for setting the serial port and baud rate for your platform.

Attach to the console using the `monitor` Make target in the AtomVM ESP32 build:

    shell$ cd .../AtomVM/src/platform/esp32
    shell$ make monitor
    Toolchain path: /work/opt/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc
    WARNING: Toolchain version is not supported: crosstool-ng-1.22.0-95-ge082013a
    ...
    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f420000, size: 1048576
    I (243) atomvm_mqtt: eFuse Two Point: NOT supported
    I (243) atomvm_mqtt: eFuse Vref: Supported
    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f430000, size: 1048576
    Found AVM partition: size: 262144, address: 0x1d0000
    Starting: gps_example.beam...
    ---
    I (165) uart: queue free spaces: 16
    I (165) nmea_parser: NMEA Parser init OK
    I (165) atomvm_gps: atomvm_gps_driver started.
    GPS started.
    GPSReading: #{altitude => {70,1000},datetime => {{2021,6,26},{13,48,57}},latitude => {XX,4984},longitude => {-YY,4822},speed => {0,2305},valid => true}
    GPSReading: #{altitude => {69,8999},datetime => {{2021,6,26},{13,48,58}},latitude => {XX,4984},longitude => {-YY,4822},speed => {0,1419},valid => true}
    GPSReading: #{altitude => {69,5000},datetime => {{2021,6,26},{13,48,59}},latitude => {XX,4984},longitude => {-YY,4822},speed => {0,572},valid => true}
    GPSReading: #{altitude => {69,5000},datetime => {{2021,6,26},{13,49,0}},latitude => {XX,4984},longitude => {-YY,4822},speed => {0,272},valid => true}
    ...
