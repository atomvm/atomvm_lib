# AtomVM MQTT Client Example Program

The `mqtt_client_example` program illustrates use of the MQTT API by connecting to a well-known MQTT broker, and sending and receiving a message on a topic every 5 seconds.  Results are displayed on the console.

> Note.  Building and flashing the `mqtt_client_example` program requires installation of the [`rebar3`](https://www.rebar3.org) Erlang build tool.

Build the example program and flash to your device:

    shell$ cd .../AtomVM/src/platforms/esp32/components/atomvm_lib/examples/mqtt_client_example
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
    Found AVM partition: size: 1048576, address: 0x110000
    Starting: mqtt_example.beam...
    ---
    ...
    I (1789) NETWORK: SYSTEM_EVENT_STA_CONNECTED received.
    I (2899) event: sta ip: 192.168.211.53, mask: 255.255.255.0, gw: 192.168.211.1
    I (2899) NETWORK: SYSTEM_EVENT_STA_GOT_IP: 192.168.211.53
    Acquired IP address: {192,168,211,53} Netmask: {255,255,255,0} Gateway: {192,168,211,1}
    I (2939) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (2939) atomvm_mqtt: MQTT_EVENT_BEFORE_CONNECT msg_id: 0
    MQTT started.
    Connected to "mqtt://mqtt.eclipseprojects.io"; subscribing to <<"atomvm/qos0">>...
    Subscribed to <<"atomvm/qos0">>.
    Publishing data on topic <<"atomvm/qos0">>
    Received data on topic <<"atomvm/qos0">>: echo
    Publishing data on topic <<"atomvm/qos0">>
    Received data on topic <<"atomvm/qos0">>: echo
    Publishing data on topic <<"atomvm/qos0">>
    Received data on topic <<"atomvm/qos0">>: echo
    Publishing data on topic <<"atomvm/qos0">>
    Received data on topic <<"atomvm/qos0">>: echo
    ...
