# MQTT Client

The AtomVM MQTT client can be used to publish messages to and subscribe to messages posted to topics on an MQTT broker.

At a high level, the following functionality is supported:

* Lifecycle and connection management, via common `start/1` and `stop/1` semantics;
* Support for TCP, TLS, Websockets, and Websockets over TLS protocols;
* Server authentication, when using TLS;
* Client Username/Password and TLS client certificate authentication;
* Publishing messages to an MQTT topic;
* Subscribing to an MQTT topic and receiving messages published to that topic;
* Unsubscribing from an MQTT topic.

The following MQTT Quality of Service (QoS) parameters may be specified when publishing messages to an MQTT topic:

* `at_most_once` The client will make a "best effort" to publish a message to the topic.  The client application will not be notified when the message is published.
* `at_least_once` The client will post at least one (but possibly more) copies of the message to the topic.  The client application will be notified (possibly multiple times) when the message has been published to the topic.
* `exactly_once` The client will post one (and only one) copy of the message to the topic.  The client application will be notified when the message has been published to the topic.

The AtomVM MQTT client is only supported on the ESP32 platform.

## Build Instructions

The AtomVM MQTT client is implemented as an AtomVM Port, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this client code, you must build an AtomVM binary image.

Instructions for building and linking an `atomvm_lib` component (Nif or Port) can be found in the [Components](components.md) document.  Please read these instructions before proceeding.

The name of the MQTT client port is `atomvm_mqtt_client`.  You should therefore edit the `component_ports.txt` file in `src/platforms/esp32/main` so that it contains a line for the AtomVM MQTT client port:

    atomvm_mqtt_client

Proceed with the instructions for building AtomVM (typically run from `src/platforms/esp32`), e.g.,

    shell$ cd .../AtomVM/src/platforms/esp32
    shell$ make
    ...
    CC .../AtomVM/src/platforms/esp32/build/atomvm_lib/ports/atomvm_mqtt_client.o
    AR .../AtomVM/src/platforms/esp32/build/atomvm_lib/libatomvm_lib.a
    ...
    To flash all build output, run 'make flash' or:
    python /opt/esp-idf-v3.3/components/esptool_py/esptool/esptool.py --chip esp32 --port /dev/tty.SLAB_USBtoUART --baud 921600 --before default_reset --after hard_reset write_flash -z --flash_mode dio --flash_freq 40m --flash_size detect 0x1000 .../AtomVM/src/platforms/esp32/build/bootloader/bootloader.bin 0x10000 .../AtomVM/src/platforms/esp32/build/atomvvm-esp32.bin 0x8000 .../AtomVM/src/platforms/esp32/build/partitions.bin

Once the AtomVM image is flashed to the ESP32 device, it includes the internal interfaces needed for communicating with the MQTT client library.

## Programmers Guide

This section provides information about how to program your Erlang or Elixir AtomVM program using the MQTT client library.

The MQTT client API is encapsulated in the `mqtt_client` Erlang module.  This module contains all functionality associated with the object lifecyle of the client instance, and through which users publish and subscribe to topics,

### Callback Architecture

Much of the interaction with this module is via callback functions or via messages delivered to a specified PID.  For example, the `start` method will start and MQTT client instance, but notification that the client is connected to the specified broker is performed asynchronously via a callback function or a well-defined message delivered to a designated process.  Similarly, notifications of publishes (QoS1) and subscriptions are performed via callbacks.

Most callback functions provide a reference to the MQTT client instance, along with other information relevant to the specific context (e.g., the MQTT topic or the message being delivered).  Generally, the return value from this function is ignored.  See the documentation for each function for the arity and semantics of specific callback function parameters.

Alternatively, users may specify a PID to which messages should be delivered when an event occurs.  For example, you can specify a PID that will be sent a message when the MQTT client connects or disconnects to the MQTT broker.

Messages are sent as tuples with the atom `mqtt' as the first element, and subsequent elements relevant to the specific context (e.g., the MQTT topic or the message being delivered).  See the documentation for each function for expected message structure for such notification messages.


### Lifecycle and connection Management

The MQTT client functionality is encapsulated in an Erlang process which you can start via the `start/1` function:

    %% erlang
    Config = #{
        ...
    },
    {ok, MQTT} = mqtt_client:start(Config).

The returned reference should be used for subsequent operations.

The input parameter to the `start/1` function is an Erlang `map` structure, containing configuration values under well-defined atomic keys that are used to initialize the connection to an MQTT broker, as well as callback functions that, if defined, will be called when certain events occur within the MQTT client.

    %% erlang
    Config = #{
        url => "mqtt://mqtt.eclipseprojects.io",
        connected_handler => fun handle_connected/1,
        disconnected_handler => fun handle_disconnected/1,
        error_handler => handle_error/2
    }

The `connected_handler` is a function (or PID) that will be notified when the MQTT client connects to the specified broker.  The instance of the MQTT client will be passed as a parameter:

    %% erlang
    handle_connected(MQTT) ->
        io:format("Connected to broker.~n"),
        ...

You can use this callback to trigger routine operations, such as publishing or subscribing.

You can use the `disconnected_handler` for notification of when the client disconnects from the MQTT broker, e.g.,

    %% erlang
    handle_disconnected(MQTT) ->
        io:format("Disconnected from broker.~n"),
        ...

Use the `error_handler` for notification of when the client detects and error from the MQTT broker, e.g.,

    %% erlang
    handle_error(MQTT, Error) ->
        io:format("Disconnected from broker.~n"),
        ...

The Error term is a 5-tuple encapsulated in the following `error()` type specification:

    -type error_type() :: esp_tls | connection_refused | undefined.
    -type connect_return_code() :: connection_accepted | protocol | id_rejected | server_unavailable | bad_username | not_authorized | undefined.
    -type tls_last_esp_err() :: integer().
    -type tls_stack_err() :: integer().
    -type tls_cert_verify_flags() :: integer().
    -type error() :: {error_type(), connect_return_code(), tls_last_esp_err(), tls_stack_err(), tls_cert_verify_flags()}.

To stop an MQTT client instance, use the `stop/1` function, supplying a reference to the MQTT client instance returned from `start/1`:

    %% erlang
    ok = mqtt_client:stop(MQTT).

### Publishing messages to an MQTT topic

You can publish a message using the `publish/4`

    %% erlang
    Topic = <<"atomvm/topic0">>,
    Message = <<"Hello!">>,
    MsgId = mqtt_client:publish(MTQQ, Topic, Message).

The above function call will publish a message to the specified topic using the MQTT QoS `at_most_once`.  Note that messages sent with `at_most_once` QoS are not subject to notification.

The return value from this function is a message id, which can be used to compare values in a `published_handler` callback.  (See below.)

> Note.  The message id returned from a call to `publish` with QoS `at_most_once` is always `0`.

You can specify `at_least_once` or `exactly_once` delivery using the `qos` option specified in the options field of the `publish/4` function:

    %% erlang
    PublishOptions = #{
        qos => exactly_once,
        published_handler => fun handle_publish/3
    },
    ok = mqtt_client:publish(MQTT, Topic, Message, PublishOptions).

You can specify a `published_handler` callback function or PID that will be notified when the message is published to the MQTT broker.

A publish handler will be passed the MQTT client instance, topic, and message id of the message that was published.

    %% erlang
    handle_publish(MQTT, Topic, MsgId) ->
        io:format("Message ~p was published to topic ~p~n", [MsgId, Topic]),
        ...

> Note. A `publish_handler` will not be notified if the the message is published with QoS `at_most_once`.

### Subscribing to an MQTT topic

Subscribe to an MQTT topic by using the `subscribe/3` function.  Specify a topic and optionally a callback for notification that the MQTT broker has acknowledged your subscription (`subscribed_handler`), as well as a callback for notification that a message has been received (`data_handler`):

    %% erlang
    Topic = <<"atomvm/topic0">>,
    SubscribeOptions = #{
        subscribed_handler = fun handle_subscribed/2,
        data_handler = fun handle_data/3
    },
    ok = mqtt_client:subscribe(MTQQ, Topic, SubscribeOptions).

The `subscribe/3` function will return `{error, already_subscribed}` if the client application is already subscribed to the specified topic.

> Note.  Due to a limitation in underlying libraries, only one subscription is allowed per topic per MQTT client.  If you would like to have more than one subscriber to the same topic, you need to create a new MQTT client instance to register your subscription.

The `subscribed_handler` will be passed the MQTT client instance and topic subscribed to:

    %% erlang
    handle_subscribed(MQTT, Topic) ->
        io:format("Subscribed to topic ~p~n", [Topic]),
        ...

The `data_handler` will be passed the MQTT client instance, topic on which the message arrived, and the message itself:

    %% erlang
    handle_data(MQTT, Topic, Message) ->
        io:format("Received message ~p on topic ~p~n", [Message, Topic]),
        ...

### Unsubscribing from an MQTT topic

Use the `unscibscribe/3` function to unsubscribe from a topic.

    %% erlang
    Topic = <<"atomvm/topic0">>,
    UnSubscribeOptions = #{
        unsubscribed_handler = fun handle_unsubscribed/2
    },
    ok = mqtt_client:unsubscribe(MTQQ, Topic, UnSubscribeOptions).

The `unsubscribe/3` function will return `{error, not_subscribed}` if the client application is not yet subscribed to the specified topic.

The `unsubscribed_handler` will be passed the MQTT client instance and topic from which the application has been unsubscribed:

    %% erlang
    handle_unsubscribed(MQTT, Topic) ->
        io:format("Unsubscribed from topic ~p~n", [Topic]),
        ...

### Advanced Configuration

TODO

#### Host and Port Settings

#### Username/Password authentication

#### Connecting via TLS

##### Client TLS Authentication

#### Buffer Settings and KeepAlive

## API Reference

To generate Reference API documentation in HTML, issue the `rebar3` target

    shell$ rebar3 edoc

from the top level of the `atomvm_mqtt` source tree.  Output is written to the `doc` directory.
