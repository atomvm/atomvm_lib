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

The `atomvm_lib` library contains the following features:

* [BME280](markdown/bme280.md)
* [BH1750](markdown/bh1750.md)
* [LEDC PWM](markdown/ledc_pwm.md)
* [LoRa](markdown/lora.md)
