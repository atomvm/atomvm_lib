# `lora_example`

Welcome to the `lora_example` AtomVM application(s).

This example application illustrates use of the Lora interface to send and receive messages between two ESP32 devices, each of which should be connected to a LoRa transceiver.  Currently, the SemTech SX127x and SX126x devices are supported.

For this application, you will need:

* Two LoRa transceivers (Semtech SX127x or SX126x);
* Two ESP32 devices, flashed with the [AtomVM](https://github.com/bettio/AtomVM) image (including the VM and core libraries), and capable of connecting via UART to your development machine;
* The [`esptool.py`](https://github.com/espressif/esptool) tool (for flashing);
* The [`git`](https://git-scm.com) version control tool;
* [Erlang/OTP](https://www.erlang.org) 21, 22, or 23, along with [`rebar3`](https://www.rebar3.org);
* A serial monitor program of your choice (e.g, [`minicom`](https://en.wikipedia.org/wiki/Minicom))

While the [IDF SDK](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/) and required tool-chains are not required, they may make life a little easier.

> Note.  These instructions assume you have flashed the AtomVM virtual machine and Erlang libraries to your ESP32 device.  For information about flashing ESP32 devices with the AtomVM virtual machine and libraries, consult the [AtomVM documentation](https://doc.atomvm.net).

## Getting Started

Connect each of your LoRa transceivers to your respective ESP32 devices.  Use the 4-wire SPI interface to connect your LoRa modem and ESP32 as follows:

| LoRa Pin | ESP32 Pin |
|----------|-----------|
| `GND` | `GND` |
| `SCLK` | `GPIO 14` |
| `MISO` | `GPIO 12` |
| `MOSI` | `GPIO 13` |
| `NCC` | `GPIO 18` |
| `DPIO0` | `GPIO 26` |
| `VCC` | `+3.3v` |

The following diagram illustrates the connected devices:

                                              \\\            //
                                            o  |||   air    ||  o
                                            | ///            \\ |
    +-------------+        +-----------+    |                   |   +-----------+       +-----------+
    |        GND  +--------+ GND      +-----+                   +----+      GND +-------+ GND       |
    |     GPIO 14 +--------+ SCLK      | antenna            antenna |      SCLK +-------+ GPIO 14   |
    |     GPIO 12 +--------+ MISO      |                            |      MISO +-------+ GPIO 12   |
    |     GPIO 13 +--------+ MOSI      |                            |      MOSI +-------+ GPIO 13   |
    |     GPIO 18 +--------+ NCC       |                            |       NCC +-------+ GPIO 18   |
    |     GPIO 26 +--------+ DPIO0     |                            |     DPIO0 +-------+ GPIO 26   |
    |      +3.3v  +--------+ VCC       |                            |       VCC +-------+ +3.3v     |
    |             |        |           |                            |           |       |           |
    +-------------+        +-----------+                            +-----------+       +-----------+
         ESP32               LoRa                                    LoRa                ESP32
                             transceiver                             transceiver

               LoRa Sender                                                      LoRa Receiver

In the example program, the LoRa sender will broadcast a simple string `AtomVM I`, where `I` is a sequence number starting at 0.  The LoRa receiver will print the received message to the console.

### Build Instructions

We will build and flash the application for the sender and receiver in several steps.

First, let's start by compiling the source code:

    shell$ rebar3 compile
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling lora_example

#### Build and Flash LoRa Receiver

Let's build and flash the LoRa receiver application on to one of your ESP32 devices.

We will first create a pruned packbeam file, with the `lora_receiver` as the entry-point into the application:

    shell$ rebar3 packbeam --prune --start lora_receiver
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling lora_example
    ===> AVM file written to : lora_example.avm

Next, we will flash the example program to your ESP32 device (replace `tty.usbserial.deviceA` with the device used for your LoRa receiver):

    shell$ rebar3 esp32_flash -p /dev/tty.usbserial.deviceA
    ...
    ===> esptool.py --chip esp32 --port /dev/tty.usbserial.deviceA --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/lora_example/_build/default/lib/lora_example.avm

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
    Found AVM partition: size: 262144, address: 0x1d0000
    Starting: lora_receiver.beam...
    ---
    Lora started.  Waiting to receive messages...

#### Build and Flash LoRa Sender

Let's build and flash the LoRa sender application on to the other one of your ESP32 devices.

We will first create a pruned packbeam file, with the `lora_sender` as the entry-point into the application (don't forget to clean the build first):

    shell$ rebar3 clean
    shell$ rebar3 packbeam --force --prune --start lora_sender
    ...
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling atomvm_lib
    ===> Analyzing applications...
    ===> Compiling lora_example
    ===> AVM file written to : lora_example.avm

Next, we will flash the example program to your ESP32 device (replace `tty.usbserial.deviceB` with the device used for your LoRa sender):

    shell$ rebar3 esp32_flash -p /dev/tty.usbserial.deviceB
    ...
    ===> esptool.py --chip esp32 --port /dev/tty.usbserial.deviceB --baud 115200 --before default_reset --after hard_reset write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x210000 .../atomvm_lib/examples/lora_example/_build/default/lib/lora_example.avm

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
    Found AVM partition: size: 262144, address: 0x1d0000
    Starting: lora_sender.beam...
    ---
    Lora started.  Sending messages...
    Sent [<<"AtomVM ">>,"0"]
    Sent [<<"AtomVM ">>,"1"]
    Sent [<<"AtomVM ">>,"2"]
    Sent [<<"AtomVM ">>,"3"]
    Sent [<<"AtomVM ">>,"4"]
    Sent [<<"AtomVM ">>,"5"]
    ...

On the console connected to your receiver, you should see:

    Received Packet: "AtomVM 0"; QoS: #{rssi => -75,snr => 13}
    Received Packet: "AtomVM 1"; QoS: #{rssi => -76,snr => 13}
    Received Packet: "AtomVM 2"; QoS: #{rssi => -78,snr => 13}
    Received Packet: "AtomVM 3"; QoS: #{rssi => -77,snr => 13}
    Received Packet: "AtomVM 4"; QoS: #{rssi => -77,snr => 13}
    Received Packet: "AtomVM 5"; QoS: #{rssi => -81,snr => 13}

If all goes according to plan, congratulations -- your two ESP32 devices are communicating using LoRa modulation!
