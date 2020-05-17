-module(httpd_example).
-export([start/0, handle_api_request/4]).

-include("logger.hrl").

start() ->
    case atomvm:platform() of
        esp32 ->
            start_wifi();
        _ -> ok
    end,
    run().

start_wifi() ->
    case network_fsm:wait_for_sta([{sntp, "pool.ntp.org"}]) of
        {ok, {Address, Netmask, Gateway}} ->
            ?LOG_INFO(
                "IP address: ~s Netmask: ~s Gateway: ~s", [
                    avm_util:address_to_string(Address),
                    avm_util:address_to_string(Netmask),
                    avm_util:address_to_string(Gateway)
                ]
            );
        Error ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Error])
    end.

-record(opts, {dht, gpio, pin}).

run() ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 2, output),
    {ok, DHT11} = dht:start(21, dht11),
    Opts = #opts{dht=DHT11, gpio=GPIO, pin=2},
    Config = [{["api"], api_handler, {?MODULE, Opts}}],
    ?LOG_INFO("Starting httpd on port 8080", []),
    case httpd:start(8080, Config) of
        {ok, _Pid} ->
            ?LOG_INFO("httpd started.", []),
            avm_util:sleep_forever();
        Error ->
            ?LOG_ERROR("An error occurred: ~p", [Error])
    end.

%%
%% API Handler implementation
%%

handle_api_request(get, ["temp"], HttpRequest, #opts{dht=DHT11}) ->
    Socket = proplists:get_value(socket, proplists:get_value(tcp, HttpRequest)),
    {ok, {Host, _Port}} = inet:peername(Socket),
    ?LOG_INFO("Temperature request from ~s", [avm_util:address_to_string(Host)]),
    {ok, {Temp, TempFractional, Hum, HumFractional}} = dht:measure(DHT11),
    {ok, [
        {temp, Temp},
        {temp_fractional, TempFractional},
        {hum, Hum},
        {hum_fractional, HumFractional}
    ]};
handle_api_request(post, ["led"], HttpRequest, #opts{gpio=GPIO, pin=Pin}) ->
    Socket = proplists:get_value(socket, proplists:get_value(tcp, HttpRequest)),
    {ok, {Host, _Port}} = inet:peername(Socket),
    QueryParams = proplists:get_value(query_params, HttpRequest),
    case proplists:get_value("led", QueryParams) of
        "on" ->
            ?LOG_INFO("Turning on LED from ~s", [avm_util:address_to_string(Host)]),
            {ok, gpio:set_level(GPIO, Pin, 1)};
        "off" ->
            ?LOG_INFO("Turning off LED from ~s", [avm_util:address_to_string(Host)]),
            {ok, gpio:set_level(GPIO, Pin, 0)};
        _ ->
            bad_request
    end;
handle_api_request(Method, Path, _HttpRequest, _HandlerOpts) ->
    ?LOG_ERROR("Unsupported method ~p and path ~p", [Method, Path]),
    not_found.
