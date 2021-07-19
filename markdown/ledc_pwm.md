## LEDC PWM

The `ledc_pwm` module provides abstractions and state management around the low-level `ledc` API provided by AtomVM, and helps to mitigate common errors with use of the low-level APIs.

The API is more "object-oriented", in that it separates the notion of a timer and channel into separate referencible entities, making it easier to manage the lifecycle of these objects.

Furthermore, the high-level API manages the relationship between a frequency set on a timer, the duty cycle resolution defined by that frequency, and the duty cycle, as set by the user.  As the details between this values is hidden, the user can set a frequency for a timer, and then specify the duty cycle as a percentage (a value between 0 and 100, inclusive), so that he or she does not need to manually compute which duty cycles are appropriate for which frequency, as one needs to do in the low-level API.

# Programming Manual

TODO

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


# LEDC PWM Example

The LEDC PWM example program illustrates use of the LEDC PWM API.  See the [README](../examples/ledc_pwm_example/README.md) for information about how to build, flash, and run this exmaple program.
