# `atomvm_lib`

This repository contains a collection of useful modules for developing programs for the AtomVM platform.

Many of these modules are "optional" and are therefore not a part of the AtomVM core libraries.

# Getting Started

The best way to use this library is to include it in your rebar3 project's `rebar.config` as a dependency:

    {deps, [
        {atomvm_lib, {git, "https://gitbub.com/fadushin/atomvm_lib.git", {branch, "master"}}}
    ]}.

Make sure to also include the `atomvm_rebar3_plugin`, so that you can generate AtomVM packbeam files and flash them to your ESP32 device.

    {plugins, [
        {atomvm_rebar3_plugin, {git, "https://github.com/fadushin/atomvm_rebar3_plugin.git", {branch, "master"}}}
    ]}.

You can then use the `packbeam` and `esp32_flash` targets to upload your application to a device.

# `atomvm_lib` modules

The `atomvm_lib` library contains the following modules.

## `ledc_pwm`

The `ledc_pwm` module provides abstractions and state management around the low-level `ledc` API provided by AtomVM, and helps to mitigate common errors with use of the low-level APIs.

The API is more "object-oriented", in that it separates the notion of a timer and channel into separate referencible entities, making it easier to manage the lifecycle of these objects.

Furthermore, the high-level API manages the relationship between a frequency set on a timer, the duty cycle resolution defined by that frequency, and the duty cycle, as set by the user.  As the details between this values is hidden, the user can set a frequency for a timer, and then specify the duty cycle as a percentage (a value between 0 and 100, inclusive), so that he or she does not need to manually compute which duty cycles are appropriate for which frequency, as one needs to do in the low-level API.

### Sample Code

The following code illustrates use of the high-level API:

    %% create a 5khz timer
    Freq = 5000.
    {ok, Timer} = ledc_pwm:create_timer(Freq).

    %% bind pin 2 to this timer in a channel
    Pin = 2.
    {ok, Channel} = ledc_pwm:create_channel(Timer, Pin).

    io:format("Frequency(hz): ~p Duty(%): ~p~n", [ledc_pwm:get_freq_hz(Channel), ledc_pwm:get_dutyp(Channel)]).

    %% set the duty cycle to 0%, and fade up to 100% over 5 seconds
    TargetDutyp = 100.
    FadeMs = 5000.
    ledc_pwm:set_dutyp(Channel, 0).
    ledc_pwm:fade(Channel, TargetDutyp, FadeMs).

### Example Program

The `ledc_pwm_example` program illustrates use of the high-level API by fading two LEDs, one automatically smoothly using the built-in fade functions, and on manually and step-wise, by setting the duty cycle percentage explicitly in code.

## `bme280`

The BME280 is a small sensor that can read temperature, humidity, and atmospheric pressure.  The chipset supports I2C and SPI interfaces.  This driver uses the AtomVM I2C interface for communicating with the BME280.  This means you can take temperature, barometric pressure, and humidity readings using two GPIO pins on your ESP32.

Developers interact with this driver by starting an instance, specifying pins for the I2C data and clock pins.  Starting an instance of the driver yeilds a reference that can be used in subsequent calls.

The primary operation in this module is the take_reading/1 function, which takes a reference to a BME280 driver, and returns a reading expressed as a tuple containing the temperature (in degrees celcius), atomspheric pressure (in hectopascals) and relative humidity (as a percentage).

Functions for reading the BME280 chip ide and version, as well as doing a soft reset of the device, are also supported.

> Note.  The BME280 sensor is a fairly dynamic sensor and can be used for many different applications (e.g., weather collection, gaming, drones, etc). The primary use-case for this driver is weather collection, which is assumed to be a low frequency operation.  Some of the BME280 applications may require additional support in this driver, which would be relatively straightforward to support in future versions.

Further information about the Bosch Sensortec BME280 can be found in the reference
documentation:
https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bme280-ds002.pdf

### Sample Code

The following code illustrates use of the high-level API:

    start() ->
        SDAPin = 21, SCLPin = 22,
        {ok, BME} = bme280:start(SDAPin, SCLPin),
        loop(BME).

    loop(BME) ->
        case bme280:take_reading(BME) of
            {ok, Reading} ->
                {Temperature, Pressure, Humidity} = Reading,
                io:format("Temperature: ~sC, Pressure: ~shPa, Humidity: ~s%RH~n", [
                    to_string(Temperature), to_string(Pressure), to_string(Humidity)
                ]);
            ErrorT ->
                io:format("Error taking reading temperature: ~p~n", [ErrorT])
        end,
        timer:sleep(5000),
        loop(BME).

    to_string({Integral, Fractional}) ->
        io_lib:format("~p.~p", [Integral, Fractional]).

### Example Program

The `bme280_example` program illustrates use of `bme280` module to retrieve temperature, atmospheric pressure, and humidity readings taken from the BME280 device attached to an ESP32 device.

* [Example](examples/bme280_example)

## `httpd`

The `httpd` module provides a simplified interface for creation of HTTP servers in your application.

> TODO write documentation
