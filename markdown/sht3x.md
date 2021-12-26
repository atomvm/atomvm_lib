## SHT3x driver

The [Sensirion](https://www.sensirion.com) [SHT3x](https://www.sensirion.com/en/environmental-sensors/humidity-sensors/digital-humidity-sensors-for-various-applications/) is a small sensor that can read temperature and humidity with a fairly high degree of accuracy.  The chipset supports the I2C interfaces, with varying levels of accuracy.  This driver uses the AtomVM I2C interface for communicating with the SHT31.
This means you can take temperature and humidity readings using two GPIO pins on your ESP32.

Developers interact with this driver by starting an instance, specifying an I2CBus instance through which I2C commands will be serialized.  Starting an instance of the driver yields a reference that can be used as input to subsequent function calls in the module.

The SHT3x driver support two modes of operation:

1. **One-Shot mode** (ideal for low-lower applications)  Applications make explicit calls to the module to obtain a measurement.
1. (Currently unimplemented) **Periodic mode** (ideal for continuous monitoring) Applications configure the device to collect measurements at periodic intervals (1/2 sec, 1s, 2s, 4s, or 10s), and to report the measurements back to the application, either via a callback function or a message delivered to a specified process.


> Note.  The SHT31 sensor is a fairly dynamic sensor and can be used for many different applications (e.g., weather collection, gaming, drones, etc).  The primary use-case for this driver is weather collection, which is assumed to be a low frequency operation.  Some of the SHT31 applications may require additional support in this driver, which would be relatively straightforward to support in future versions.

Further information about the Sensirion SHT3x can be found in the [SHT3x datasheet](../assets/Sensirion_Humidity_Sensors_SHT3x_Datasheet_digital.pdf).

# Programming Manual

The SHT3x Driver is designed to provide a simple, easy to use interface for taking temperature and humidity measurements for your AtomVM application.  This section describes the SHT3x driver API, as encapsulated in the `sht3x` Erlang module.

## Lifecycle

An instance of the SHT3x driver is created using an instance of the I2CBus service.  Start by initializing an I2Cbus using the `i2c_bus` module, specifying the `sda` and `scl` pins in a `map` structure:

    {ok, I2CBus} = i2c_bus:start(#{
        sda => 21,
        scl => 22
    }),

You can then use the `sht3x:start[_link]/1` or `sht3x:start[_link]/2` functions to start an instance of the SHT3x driver:

    {ok, BME} = sht3x:start_link(I2CBus),
    ...

The return value includes a reference to the driver, which is used in subsequent operations.

> Note.  The I2CBus instance may be used to initialize other I2C devices that are multiplexed on the same I2C bus.

The arity-2 version of the `start[_link]` function allows additional parameters to be specified, in a manner described in more detail below.

To stop an instance of the driver and free any resources in use, use the `sht3x:stop/1` function.

## Configuration

The SHT3x driver can be configured via an optional `map` structure passed in to the `start[_link]/2` function.


| Key | Value | Default | Description |
|-----|-------|---------|-------------|
| `repeatability` | `high \| medium \| low` | `high` | The repeatability of the measurement.  Higher repeatability will result in less noise in the measurement, more time to take the measurement, and higher current consumption. |
| `clock_stretching` | `enabled \| disabled` | `disabled` | Whether I2C standard clock stretching is enabled. |

## Single-Shot Readings

Single-shot readings are obtained via a call to the `sht3x:take_reading/0` function.

Readings are represented as a tuple containing the temperature (in degrees celsius) and relative humidity, as a percentage.  Each value contains an integral and fractional part, with the fractional part expressed as a ratio.

The SHT31x supports a 0.01% relative humidity resolution, and a 0.015C temperature resolution.

For example,

    %% erlang
    {ok, {Temperature, Humidity}} = sht3x:take_reading(SHT),
    io:format("Temperature: ~p, Humidity: ~p~n", [Temperature, Humidity]),
    ...

prints something like the following to the console:

    Temperature: {19,{79,1000}}, Humidity: {31,{91,100}}

I.e., 19.079C, 31.91% relative humidity.

## Periodic Readings

TODO (currently unimplemented)

## Soft Reset

TODO (currently unimplemented)

## Heater

TODO (currently unimplemented)

# SHT3x Example

The SHT3x driver includes an example program illustrating use of the SHT3x driver.  See the [README](../examples/sht3x_example/README.md) for information about how to build, flash, and run this example program.
