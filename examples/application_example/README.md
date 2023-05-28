application_example
===================

This example program illustrates how to build and deploy an OTP application into AtomVM.

You can deploy an OTP application into AtomVM by implementing your application using [OTP design principles](https://www.erlang.org/doc/design_principles/users_guide.html), including implementations of the [application](https://www.erlang.org/doc/man/application.html), [supervisor](https://www.erlang.org/doc/man/supervisor.html), and various `gen_` behaviors (e.g., [gen_server](https://www.erlang.org/doc/man/gen_server.html), [gen_statem](https://www.erlang.org/doc/man/gen_statem.html), and [gen_event](https://www.erlang.org/doc/man/gen_event.html)).

In addition, you can deploy your application and its dependencies (as defined in the application specification file) automatically into AtomVM without having to write a `start` entrypoint.

Instead, all you need to do is using the `init` module as the start module, and all applications and their dependencies will be automatically loaded for you.

In this example, we create an OTP `application`, containing a `supervisor` that supervises a single worker process.  This worker simply sends a `tick` message to itself once a second.  After 5 ticks, the process will exit with a `boom`.  The supervisor will restart the process, and the ticks will continue.

# Building the example application

Build the packbeam for this application, specifying `init` as the start module.

    $ rebar3 packbeam -p -s init

> Note that `init` is not an application that is defined in this example program, but instead is defined in the AtomVM (`atomvm_lib`) framework.

If you have the [packbeam](https://github.com/atomvm/atomvm_packbeam) executable available, you can list the contents of the generated AVM file:

    $ packbeam list _build/default/lib/myapp.avm
    init.beam * [1832]
    myapp_worker.beam [740]
    myapp_sup.beam [704]
    myapp_app.beam [564]
    myapp/priv/application.bin [280]
    init/priv/start.boot [56]
    avm_env.beam [4980]
    avm_application_controller.beam [3800]
    avm_application.beam [812]
    atomvm_lib/priv/application.bin [712]

# Running the example application

You can run this example program on the `generic_unix` and `esp32` platforms.

## Running on the `generic_unix` platform

Use the `atomvm` executable to run the generated AVM file:

    $ atomvm _build/default/lib/myapp.avm
    Starting myapp_app ...
    Starting myapp_sup ...
    Application myapp started
    tick
    tick
    tick
    tick
    tick
    CRASH
    ======
    pid: <0.4.0>

    Stacktrace:
    [{myapp_worker,handle_info,2,[{file,".../examples/application_example/src/myapp_worker.erl"},{line,59}]},{gen_server,loop,2,[{file,".../AtomVM/libs/estdlib/src/gen_server.erl"},{line,399}]}]

    cp: #CP<module: 13, label: 10, offset: 23>

    x[0]: exit
    x[1]: boom
    x[2]: {2,2,178,2,[{3,2284},{13,131}]}

    Stack
    ------

    #CP<module: 3, label: 90, offset: 24>
    []
    []
    {state,5}
    []
    []
    {state,undefined,myapp_worker,{state,5}}
    #CP<module: 3, label: 110, offset: 0>


    Registers
    ----------
    x[0]: exit
    x[1]: boom
    x[2]: {2,2,178,2,[{3,2284},{13,131}]}
    x[3]: []
    x[4]: []
    x[5]: []
    x[6]: []
    x[7]: []
    x[8]: []
    x[9]: []
    x[10]: []
    x[11]: []
    x[12]: []
    x[13]: []
    x[14]: []
    x[15]: []


    Mailbox
    --------


    **End Of Crash Report**

## Running on the `esp32` platform

You can flash the application to your ESP32 device by using the `esp32_flash` target:

    $ rebar3 esp32_flash -p /dev/ttyUSB0
    ...

You can then attach to the console using `minicom` or equivalent serial program:

    I (0) cpu_start: Starting scheduler on APP CPU.

        ###########################################################

           ###    ########  #######  ##     ## ##     ## ##     ##
          ## ##      ##    ##     ## ###   ### ##     ## ###   ###
         ##   ##     ##    ##     ## #### #### ##     ## #### ####
        ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ##
        #########    ##    ##     ## ##     ##  ##   ##  ##     ##
        ##     ##    ##    ##     ## ##     ##   ## ##   ##     ##
        ##     ##    ##     #######  ##     ##    ###    ##     ##

        ###########################################################

    I (849) AtomVM: Starting AtomVM revision 0.6.0-dev+git.0c549a28
    I (859) AtomVM: Loaded BEAM partition main.avm at address 0x210000 (size=1048576 bytes)
    I (869) otp_socket: Initialized AtomVM socket.
    I (879) atomvm_nvs_reset: NVS Reset task is running. (pin=0 invert_pin=false)
    I (889) AtomVM: Found startup beam init.beam
    I (889) AtomVM: Loaded BEAM partition lib.avm at address 0x1d0000 (size=262144 bytes)
    I (899) AtomVM: Starting init.beam...
    ---
    Starting myapp_app ...
    Starting myapp_sup ...
    Application myapp started
    tick
    tick
    tick
    ...
