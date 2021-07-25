
# LoRa (SX127X) Driver

The SemTech [SX127X](https://semtech.my.salesforce.com/sfc/p/#E0000000JelG/a/2R0000001Rbr/6EfVZUorrpoKFfvaF_Fkpgp5kzjiNyiAbqcpqh9qSjE) is a compact wireless [LoRa](https://en.wikipedia.org/wiki/LoRa) modem, capable of low-power wireless communication over long distances for IoT applications.  The SX127X integrated circuit can be integrated directly into your circuit designs, but is more typically supplied via a module for integration with your projects.

The `atomvm_lib` LoRa driver provides support for integrating the SX127X chipset with [AtomVM](https://github.com/bettio/AtomVM) applications running on the [ESP32](https://www.espressif.com/en/products/socs/esp32) devices.  LoRa provides and alternative to the built-in WiFi and Bluetooth radios on-board the ESP32 and is generally more useful for battery powered applications that are out of range of typical WiFi access points.

> Note.  Testing has primarily utilized SX127X modules, such as the [NiceRF LoRa 1276](https://d3s5r33r268y59.cloudfront.net/datasheets/3118/2014-08-26-09-59-21/LoRa1276%20datasheet.pdf).

The SX127X supports the 4-pin SPI interface, and the `atomvm_lib` LoRa driver makes use of this interface to communicate with the SX127X modem to send and receive data.  Users may select any supported SPI GPIO pins on your device to integrate with the modem.  Additional pins for the SX127X `dpio_0` (for receive) and `reset` pins may also be used.

# Programming Manual

The `atomvm_lib` LoRa driver is designed to provide a simple, interoperable, and easy to use interface for sending and receiving data using LoRa modulation for AtomVM applications.  This section describes the `atomvm_lib` LoRa driver API, as encapsulated in the `lora` Erlang module.

## Lifecycle

An instance of the `atomvm_lib` LoRa driver is created via the `lora:start/2` function.  This function takes an SPI instance, which must be configured and created separately, and a set of LoRa-specific configuration, described in more detail below.

The SPI AtomVM port must be instantiated separately from the LoRa driver, as users may have application-specific requirements for SPI integration (e.g., multiple SPI interfaces on the same SPI Bus).

For example, to configure the 4-wire SPI interface to use the ESP32 HSPI IOMUX pins, you can use the following configuration:

    %% erlang
    SPIConfig = [
        {bus_config, [
            {miso_io_num, 12},
            {mosi_io_num, 13},
            {sclk_io_num, 14}
        ]},
        {device_config, [
            {spi_clock_hz, 1000000},
            {spi_mode, 0},
            {spi_cs_io_num, 18},
            {address_len_bits, 8}
        ]}
    ],
    SPI = spi:open(SPIConfig),
    ...

The SPI instance can now be used to start the `atomvm_lib` LoRa driver.

> Note.  For more information about the AtomVM SPI interface, see the AtomVM documentation.

In addition to an SPI instance, the driver is initialized with a map, containing LoRa configuration for the instance, e.g.,

    %% erlang
    LoraConfig = #{
        frequency => freq_915mhz,
        bandwidth => bw_125khz,
        dio_0 => 26,
        receive_handler => fun handle_receive/3
    },
    {ok, Lora} = lora:start(SPI, LoraConfig),
    ...

For more information about the LoRa configuration, see the [Configuration](#Configuration) section, below.

The return value includes a reference to the driver, which is used in subsequent operations.

To delete an instance of the driver, use the `lora:stop/1` function.

> Note.  This function is not well tested and its use may result in a memory leak.

## Configuration

The `atomvm_lib` LoRa driver is initialized with a map containing keys and values used to configure the driver.  The keys and values in this map are summarized in the following table:

| Key | Type | Required | Default Value | Description |
|-----|-------|----------|---------------|-------------|
| `frequency` | `freq_169mhz \| freq_433mhz \| freq_868mhz \| freq_915mhz \| non_neg_integer()` | no  | `freq_915mhz` | Wireless frequency (typically determined by geographic region). |
| `bandwidth` | `bw_7_8khz \| bw_10_4khz \| bw_15_6khz \| bw_20_8khz \| bw_31_25khz \| bw_41_7khz \| bw_62_5khz \| bw_125khz \| bw_250khz \| bw_500khz` | no  | `bw_125khz` | Signal bandwidth (increased bandwidth improve throughput, at the expense of range). |
| `tx_power` | `2..17` | no  | `2` | Transmission power.  More power will increase range, at the expense of power consumption. |
| `spreading_factor` | `6..12` | no  | `7` | LoRa spreading factor.  This value must be known in advance by the sender and receiver.  The higher the spreading factor, the better the range, at the expense of speed of transmission. |
| `preamble_length` | `6..65535` | no  | `8` |  |
| `coding_rate` | `cr_4_5 \| cr_4_6 \| cr_4_7 \| cr_4_8` | no  | `cr_4_5` | The error coding rate. |
| `header_mode` | `implicit \| explicit` | no  | `explicit` | LoRa header mode. |
| `sync_word` | `TODO` | no  | `0x12` | TODO. |
| `lna_gain` | `lna_0 \| lna_1 \| lna_2 \| lna_3 \| lna_4 \| lna_5 \| lna_6 \| auto` | no  | `auto` | TODO. |
| `enable_crc` | `true \| false` | no  | `true` | TODO. |
| `invert_iq` | `true \| false` | no  | `false` | TODO. |
| `dio_0` | `non_neg_integer()` | yes, if receiving messages |  | ESP32 Pin used to signal receipt of a message.  This pin should be connected to the DIO_0 pin on the LoRa modem.  See [Receiving Data](#Receiving_Data) below for more information. |
| `receive_handler` | `fun(Lora::pid(), Message::term(), QoS::qos()) -> any().` | likely, if receiving messages |  | Callback function that is invoked when a message is received.  See [Receiving Data](#Receiving_Data) below for more information. |
| `reset` | `non_neg_integer()` | no |  | ESP32 Pin used to reset the modem on initialization.  Resetting the pin may be desireable in cases where the modem contains stale information that needs to be reset on a regular basis. |

### Choosing LoRa parameters

Choosing the right parameters for LoRa communications can seem to be more of an art that a science, and is dependent on a number of factors, including:

* Your location, and the governing laws around allowable frequencies, message time to deliver, and so forth;
* The latency requirements for your application, i.e., how quickly you want to deliver messages;
* The packet size requirements for your application, i.e., how large you want your messages to be;
* The range of your transmissions, i.e., the distance between your senders and receivers, and how many obstacles there are in your area.

TODO: Provide guidance for parameter selection

## Broadcasting Packets

To broadcast a packet using the LoRa driver, use the `lora:broadcast/2` function, supplying the `Lora` instance created via `lora:start/2`, and the message packet.  The packet may be a string (list of integers), binary, or I/O list.  E.g.,

    %% erlang
    Packet = [<<"AtomVM ">>, integer_to_list(I)],
    ok = lora:broadcast(Lora, Packet),
    ...

An `ok` return value indicates that the message has been broadcast, bit not necessarily that any other devices have received the message.

## Receiving Packets

Messages are received asynchronously in the `atomvm_lib` LoRa driver and can be delivered to applications via a callback function supplied to the driver at the time of initialization.

In order to receive messages, however, you must specify the pin on the ESP32 that is connected to the `dio_0` pin on the LoRa modem, as the arrival of a message will trigger an interrupt when the `dio_0` pin is rising.  E.g, in the driver configuration map, you might have an entry like:

    %% erlang
    dio_0 => 26

In addition, you should specify a 3-ary callback function, which will be called when a packet arrives.  This function is specified in the configuration map using the `receive_handler` key, e.g.,

    %% erlang
    receive_handler => fun handle_receive/3

This function takes three parameters:

* `Lora` a reference to the LoRa instance;
* `Packet` the received packet (as a string, i.e., list of bytes);
* `QoS` the quality of service, as a map, containing the received signal strength indicator (`rssi`) and signal-to-noise ratio (`snr`).

For example, this handler simply prints the received packet and quality of service to the console:

    handle_receive(_Lora, Packet, QoS) ->
        io:format("Received Packet: ~p; QoS: ~p~n", [Packet, QoS]).

# LoRa Example

The `atomvm_lib` LoRa driver includes an example program illustrating use of the driver.  See the [README](../examples/lora_example/README.md) for information about how to build, flash, and run this example program.
