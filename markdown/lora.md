# AtomVMLib LoRa Drivers

The `atomvm_lib` LoRa drivers provides support for integrating [LoRa](https://en.wikipedia.org/wiki/LoRa) modules with [AtomVM](https://github.com/atomvm/AtomVM) applications running on the [ESP32](https://www.espressif.com/en/products/socs/esp32) devices.  LoRa provides and alternative to the built-in WiFi and Bluetooth radios on-board the ESP32 and is generally better for battery powered applications that are out of range of typical WiFi access points.

Currently, the following two LoRa modules are supported:

* SemTech [SX127x](../assets/DS_SX1276-7-8-9_W_APP_V7.pdf)
* SemTech [SX126x](../assets/DS_SX1261-2_V1.2.pdf)

The drivers for these LoRa modules are described below in more detail.

# LoRa (SX127x) Driver

The SemTech [SX127x](../assets/DS_SX1276-7-8-9_W_APP_V7.pdf) is a compact wireless [LoRa](https://en.wikipedia.org/wiki/LoRa) modem, capable of low-power wireless communication over long distances for IoT applications.  The SX127x integrated circuit can be integrated directly into your circuit designs, but is more typically supplied via a module for integration with your projects.

The SX127X supports the 4-pin SPI interface, and the `atomvm_lib` LoRa driver makes use of this interface to communicate with the SX127X modem to send and receive data.  Users may select any supported SPI GPIO pins on your device to integrate with the modem.  Additional pins for the SX127x `dpio_0` (Interrupt for receive) and `reset` pins may also be used.  See the [Configuration](#Configuration) section below, for more information.

The `atomvm_lib` SX127x LoRa driver requires that the SPI `address_len_bits` device config be set to `8`.

> Note.  Testing has primarily utilized SX127x modules, such as the [NiceRF LoRa 1276](../assets/LoRa127X_100mW_LoRa_Wireless_Transceiver_Module_V3.0.pdf).

# LoRa (SX126x) Driver

The SemTech SemTech [SX126x](../assets/DS_SX1261-2_V1.2.pdf) is a compact wireless [LoRa](https://en.wikipedia.org/wiki/LoRa) modem, capable of low-power wireless communication over long distances for IoT applications.  The SX126x integrated circuit can be integrated directly into your circuit designs, but is more typically supplied via a module for integration with your projects.

The SX126x supports the 4-pin SPI interface, and the `atomvm_lib` LoRa driver makes use of this interface to communicate with the SX127X modem to send and receive data.  Users may select any supported SPI GPIO pins on your device to integrate with the modem.  Additional pins for the SX126x `dpio_1` (IRQ for receive), `busy`, and `reset` pins may also be used.  See the [Configuration](#Configuration) section below, for more information.

The `atomvm_lib` SX126x LoRa driver requires that the SPI `address_len_bits` device config be set to `0`.

> Note.  Testing has primarily utilized SX126x modules, such as the [NiceRF LoRa 1262](../assets/LoRa126X_160mW_Low_Power_Consumption_Wireless_Transceiver_Module_V2.1.pdf).

# Programming Manual

The `atomvm_lib` LoRa driver is designed to provide a simple, interoperable, and easy to use interface for sending and receiving data using LoRa modulation for AtomVM applications.  This section describes the `atomvm_lib` LoRa driver API, as encapsulated in the `lora` Erlang module.

## Lifecycle

An instance of the `atomvm_lib` LoRa driver is created via the `lora:start/2` function.  This function takes an SPI instance, which must be configured and created separately, and a set of LoRa-specific configuration, described in more detail below.

The SPI AtomVM port must be instantiated separately from the LoRa driver, as users may have application-specific requirements for SPI integration (e.g., multiple SPI interfaces on the same SPI Bus).

For example, to configure the 4-wire SPI interface to use the ESP32 HSPI IOMUX pins, you can use the following configuration:

    %% erlang
    SPIConfig = #{
        bus_config => #{
            miso_io_num => 19,
            mosi_io_num => 27,
            sclk_io_num => 5
        },
        device_config => #{
            my_sx127x => #{
                spi_cs_io_num => 18,
                spi_clock_hz => 1000000,
                address_len_bits => 8,
                mode => 0
            }
        }
    },
    SPI = spi:open(SPIConfig),
    ...

> IMPORTANT. The SX127x driver requires that the SPI `address_len_bits` device config be set to `8`.  For the SX126x module, set the `address_len_bits` device config to `0`.

The SPI instance can now be used to start the `atomvm_lib` LoRa driver.

> Note.  For more information about the AtomVM SPI interface, see the AtomVM documentation.

In addition to an SPI instance, the driver is initialized with a map, containing LoRa configuration for the instance, e.g.,

    %% erlang
    LoraConfig = #{
        spi => spi,
        frequency => freq_915mhz,
        bandwidth => bw_125khz,
        irq => 26,
        receive_handler => fun handle_receive/3
    },
    {ok, Lora} = lora:start(LoraConfig),
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
| `preamble_length` | `6..65535` | no  | `8` | Number of symbols in the packet preamble used to synchronize the receiver with the incoming message.  A longer preamble can result in better receiver sensitivity, at the expense of a longer duty cycle.  Note that some jurisdictions impose strict limits on the length of a message duty cycle.  If the sender and receiver are not configured to use the same preamble length, the receiver should be set to the maximum allowable value. |
| `coding_rate` | `cr_4_5 \| cr_4_6 \| cr_4_7 \| cr_4_8` | no  | `cr_4_5` | The forward error [correction code rate](https://en.wikipedia.org/wiki/Code_rate).  This value is expressed as a ratio of non-redundant data in the message payload.  (Note that the header, if present, uses a error coding rate of 4/8) |
| `header_mode` | `implicit \| explicit` | no  | `explicit` | LoRa header mode.  Only use implicit header mode if the payload length, error correction coding rate, and presence of the CRC is known in advance.  This is uncommon. |
| `sync_word` | `0x00..0xFF` | no  | `0x12` | 8-bit value used by the receiver to synchronize the start of a message.  This value must be in agreement between the sender and receiver.  Note that the value `0x34` is reserved for LoRaWAN networks. |
| `lna_gain` | `lna_1 \| lna_2 \| lna_3 \| lna_4 \| lna_5 \| lna_6 \| auto` | no  | `auto` | Low Noise Amplifier gain control.  This setting adjusts the sensitivity of the SX127X receiver.  `lna_1` is maximum gain, and `lan_6` is minimum gain.  Most applications use the `auto` gain feature of the SX127X. |
| `enable_crc` | `true \| false` | no  | `true` | TODO. |
| `invert_iq` | `true \| false` | no  | `false` | TODO. |
| `irq` | `non_neg_integer()` | yes, if receiving messages |  | ESP32 Pin used to signal receipt of a message.  This pin should be connected to the DIO_0 pin on the LoRa modem.  See [Receiving Data](#Receiving_Data) below for more information. |
| `receive_handler` | `fun(Lora::pid(), Message::term(), QoS::qos()) -> any().` | likely, if receiving messages |  | Callback function that is invoked when a message is received.  See [Receiving Data](#Receiving_Data) below for more information. |
| `reset` | `non_neg_integer()` | no |  | ESP32 Pin used to reset the modem on initialization.  Resetting the pin may be desireable in cases where the modem contains stale information that needs to be reset on a regular basis. |
| `busy` | `non_neg_integer()` | no |  | ESP32 Pin used to signal whether the SX126x module is busy and whether the driver should wait before sending an SPI command.  Use of this pin is optional but recommended. |

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
    irq => 26

In addition, you should specify a 3-ary callback function, which will be called when a packet arrives.  This function is specified in the configuration map using the `receive_handler` key, e.g.,

    %% erlang
    receive_handler => fun handle_receive/3

This function takes three parameters:

* `Lora` a reference to the LoRa instance;
* `Packet` the received packet (as a string, i.e., list of bytes);
* `QoS` the quality of service, as a map, containing the received signal strength indicator (`rssi`) and signal-to-noise ratio (`snr`).

For example, this handler simply prints the received packet and quality of service to the console:

    %% erlang
    handle_receive(_Lora, Packet, QoS) ->
        io:format("Received Packet: ~p; QoS: ~p~n", [Packet, QoS]).

## Putting the device to sleep

For low power applications, it may be useful to put the modem into sleep mode when application puts the MCU to sleep.  Doing so can greatly enhance battery life.

    %% erlang
    ...
    lora:sleep(Lora),
    ...

# LoRa Example

The `atomvm_lib` LoRa driver includes an example program illustrating use of the driver.  See the [README](../examples/lora_example/README.md) for information about how to build, flash, and run this example program.
