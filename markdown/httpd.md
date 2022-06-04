## HTTPd Server

The HTTPd server provides a simple HTTP server for your AtomVM applications.  Using this server, you can service HTTP requests on your AtomVM instance, allowing you to serve files (e.g, HTML, CSS, Javascript, JPEG, etc), as well as service REST-based APIs over the HTTTP protocol.  You can also implement handlers for websockets, allowing you to efficiently communicate between you application and a web application running in the browser.

At a high level, this server supports the following features:

* HTTP 1.1 support, per [RFC 2616](https://datatracker.ietf.org/doc/html/rfc2616);
* Ability to efficiently serve files embedded in AtomVM AVM files;
* An Erlang API for implementing your own REST-based APIs;
* An Erlang API for implementing components and send and receive data over the [WebSocket](https://datatracker.ietf.org/doc/html/rfc6455) protocol.

The HTTPd server is designed around a callback architecture, whereby users implement behaviors to handle various requests into the HTTP server.  This architecture allows developers to focus on the logic of their applications, as opposed to the nitty gritty details of the HTTP protocol, while still providing access to contextual information about the request, including:

* The HTTP verb used in the request, expressed as an atom (`get`, `post`, `delete`, etc.);
* The URI path used in the request, expressed as a list of binaries;
* URL parameters, if present, expressed as an Erlang map from strings to string;
* The HTTP headers provided by the client, expressed as an erlang map from strings to strings;
* A reference to the underlying TCP/IP socket on which the request was sent.

In addition to the `httpd` server and its associated callbacks, this project also includes two specialized HTTP handlers to simplify the development of your application:

* HTTP File handler -- This handler supports the loading of regular files (e.g., HTML, CSS, Javascript, JPG, etc) from AVM files deployed into your application.  Simply specify the application in which these files reside, and the HTTP file handler will manage all of the file loading and retrieval for you.
* HTTP API handler -- This handler allows users to write simple REST-based API calls in response to HTTP GET, POST, PUT, and other HTTP verbs.
* HTTP WebSocket handler -- This handler allows users to send and receive data over the WebSocket protocol.

> Note.  The `httpd` server does not currently support TLS communication, so all HTTP requests and responses will be sent in the clear.  Use this program at your own risk, and only on a network you can trust.

# Programming Manual

This section provides a detailed description of how to use the HTTPd server modules.

## Using the HTTPd server

To use the HTTPd server, add the `atomvm_lib` to your list of `rebar3` or `mix` dependencies.

For example:

    %% rebar3
    {deps, [atomvm_lib]}.
    {plugins, [atomvm_rebar3_plugin]}.

Use the `rebar3_atomvm_plugin` targets to build and flash your application, as per the [AtomVM Rebar3 documentation](https://github.com/atomvm/atomvm_rebar3_plugin).  E.g.,

    shell$ rebar3 packbeam -p
    ...
    shell$ rebar3 esp32_flash -p /dev/ttyUSB0

## Types

The `httpd` module provides the following type definitions, which are useful in the remainder of this document:

    -type method() :: get | post | put | delete.
    -type content_type() :: string().
    -type path() :: list(binary()).
    -type query_params() :: #{
        binary() := binary()
    }.
    -type http_request() :: #{
        method := method(),
        path := path(),
        uri := string(),
        query_params := query_params(),
        headers := #{binary() := binary()},
        body := binary(),
        socket := term()
    }.
    -type handler_config() :: #{
        module := module(),
        module_config := term()
    }.
    -type config() :: [{path(), handler_config()}].
    -type portnum() :: 0..65536.

## Lifecycle

To start an instance of the HTTPd server, use the `httpd:start_link/2` function, providing port number and a reference to configuration used to initialize the server:

    %% erlang
    Port = 8080,
    Config = ...
    {ok, Httpd} = httpd:start_link(Port, Config),
    ...

If successful, the HTTPd server should be listening on the specified port for client connections.

> Note.  The configuration for the HTTPd server is described in more detail below.

To stop an instance of the HTTPd server, use the `httpd:stop/1` function, proving a reference to the HTTPd server obtained via `httpd:start_link`:

    %% erlang
    ok = httpd:stop(Httpd).
    ...

Stopping an HTTPd server will stop the listening socket and free any resources in use by the server.

## Configuration

Configuration of the HTTPd server is driven via a properties list (i.e., and ordered list of `{Key, Value}` pairs), which assigns HTTP handlers to URL path prefixes.

Each entry in this properties list is a tuple of the form:

    {path(), #{handler => module(), handler_config => term()}}

where `path()` is a list of binaries, ostensibly the initial parts of a URL path that are separated by forward slashes.  For example, if the URL in a request is `http://atomvm/api/system_info`, then the full path of that URL would be represented in the Erlang list `[<<"api">>, <<"system_info">>]`, and a path prefix would be `[<<"api">>]`.  Note that a URL with an empty path, e.g., `http://atomvm/` is expressed by the empty list (`[]`).

The second element of an entry in the configuration properties list is a map which contains the name of a module that implements the `http_handler` behavior, together with a handler-specific term used to configure the handler.

The configuration properties list is used to select a handler for a given request.  When an HTTP request is made on the server, the full request URL path is split into a list of path components (i.e., a list of binaries).  The HTTPd server will then search for the first entry in the configuration properties list which contains a path prefix that matches the URL request path.  If it finds a handler with that prefix, it will pass off the request to the handler for processing, and return the reply generated by the handler.  If no handler can be found for a given HTTP request, then the HTTPd server will respond to the client with an HTTP 404 (NotFound) HTTP error code.

When a handler is selected for processing, the path prefix designated in the configuration properties list is stripped from the path and passed off the the handler.  This way, handlers do not need to know the context under which they were invoked, making them more suitable for re-use in other applications.  Note that the full URL path is available to the handler via other contextual information, but is generally not needed.

In the following example, two handlers are specified, the first of which is triggered when users invoke URLs starting with the `/api` path, and the second of which is triggered for any other URLs.  The first path will use the HTTP API handler to service requests, and the second will use the HTTP File handler to service requests.

> Note that the handler configuration syntax varies according to the handler type being used.  The File, API, and Websocket handlers are described in sections below.

    %% erlang
    Config = [
        {[<<"api">>]], #{
            handler => http_api_handler,
            handler_config => #{
                module => ?MODULE,
                args => #args{}
            }
        }},
        {[], #{
            handler => http_file_handler,
            handler_config => #{
                app => ?MODULE
            }
        }}
    ]

In the above example, if an HTTP request is made on the server with the URL path `/api/system_info`, then the first handler will be used to service the request (in this case, the `http_api_handler`).  Any other request will be handled by the `http_file_handler`.

Recall that the path (prefix) is stripped when the path is passed off to the configured HTTP handler.  Thus, for example, the `http_api_handler` will only see `[<<system_info>>]` in the path that is provided to it, since the `<<"api">>` element is stripped off.  In the above `http_file_handler` example, the path prefix is empty, so the `http_file_handler` will see the full path provided in the request.

> Note.  The empty path (`[]`) will match any URL path, and is usually specified last in the configuration properties list, if it is specified at all.  You can think of this entry as the "default" handler, if no other handler matches the request URL path.


## HTTP Handlers

The HTTP server ships with a collection of built-in handlers that make development of an HTTP server easy.  These handlers are described in this section.

### The HTTP File Handler

The HTTP File handler is used to serve files (such as HTML, CSS, Javascript, etc) that are embedded in AtomVM AVM files.  The HTTP File handler can service any kind of file, but typically they are HTML, CSS, Javascript, and image files, on order for your device to service a rich web client experience.

When plain data (non-BEAM) files are embedded in an AVM file, they are generally prefixed with an application name, under which the AtomVM application is built.

For example, if an AtomVM application (e.g., `httpd_example`) contains a priv directory with the following contents:

    priv/favicon.ico
    priv/index.html
    priv/js/app.js
    priv/w3.css
    priv/js/lib/jquery-3.1.1-min.js

then the `packbeam` tool (as well as the `rebar3_atomvm_plugin` and `ExAtomVM` mix plugin) will generate an AtomVM AVM with the files prefixed with the application name.  For example:

    shell$ packbeam list _build/default/lib/httpd_example.avm
    ...
    httpd_example/priv/favicon.ico [15410]
    httpd_example/priv/index.html [7202]
    httpd_example/priv/w3.css [32606]
    httpd_example/priv/js/app.js [2998]
    httpd_example/priv/js/lib/jquery-3.1.1-min.js [86715]
    ...

The application name therefore provides a kind of key into your application data, and is used internally to locate the data files embedded in the AVM file.

#### Configuration

The HTML File handler is configured through a HTTP handler configuration entry in the HTTPd server, as described above.  This configuration contains a map with the following elements:

| Key | Type | Value |
|-----|------|-------|
| `handler` | `module()` | `http_file_handler` |
| `handler_config` | `map()` | See below. |

To use the HTML File handler, specify the `http_file_handler` in the `handler` configuration in the HTTPd server configuration for your HTTPd server instance.

The `handler_config` map contains the following elements:

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `app` | `atom()` (application name) | yes | The BEAM application module which contain the data files |

For example:

        {[], #{
            handler => http_file_handler,
            handler_config => #{
                app => ?MODULE
            }
        }}

With this example configuration, an HTTP request with the path `/index.html` will yield the `index.html` file embedded in the `httpd_example` application AVM file.

> Note. The retrieval of file data from AVM files is generally quite efficient, with minimal impact on RAM resources on an ESP32 device.  In general you should not be limited by file sizes, except for the spaced used by your AVM file and network overhead associated with the transfer of the file data to the HTTP client.

#### A Special Note about Paths

Recall from above that the URL path prefix used in the HTTPd server configuration is stripped off before being passed off to the appropriate handler.  In the above example, the path prefix is empty, so the HTTP File handler, in this case, will see the full path.  However, if a path prefix had been specified, the HTTP File handler will only see the path "below" that path.  As a consequence, you may specify multiple file handlers in your configuration, but you will need to specify different URL path prefixes for each handler.

Note also that in the case of the HTTP File handler, the handler does not account for the `<application-name>/priv` path in the associated AVM file.

A few examples might be of some assistance:

| URL Prefix (in config) | URL Path | Application Name | AVM File Search Path | HTTP Result |
|------------------------|----------|------------------|---------------|--------|
| `[]` | `/index.html` | `httpd_example` | `httpd_example/priv/index.html` | 200 (`index.html`) |
| `[]` | `/js/app.js` | `httpd_example` | `httpd_example/priv/js/app.js` | 200 (`app.js`) |
| `[]` | `/does-not-exist` | `httpd_example` | `httpd_example/priv/does-not-exist` | 404 (NOT FOUND) |
| `[]` | `/priv/index.html` | `httpd_example` | `httpd_example/priv/priv/index.html` | 404 (NOT FOUND) |
| `[<<"foo">>]` | `foo/index.html` | `httpd_example` | `httpd_example/priv/index.html` | 200 (`index.hml`) |
| `[]` | `foo/index.html` | `httpd_example` | `httpd_example/priv/foo/index.html` | 404 (NOT FOUND) |

### The HTTP API Handler

The HTTP API Handler is used to allow application developers to provide a REST-based API into their applications.  Application must implement their own callback modules to use this handler, but by doing so, much of the complexity of writing HTTP applications is abstracted away, allowing developers to focus on the application logic, instead of the details of the HTTP protocol.

#### The `http_api_handler` Behavior

In order to make use of the HTTP API Handler, applications must implement the `http_api_handler` behavior.  This behavior is implemented via the following callback function:

    -type json_term() :: #{atom() := true | false | atom() | integer() | string() | binary() | list(json_term()) | json_term()}.

    -callback handle_api_request(Method :: httpd:method(), PathSuffix :: httpd:path(), HttpRequest :: httpd:http_request(), Args :: term()) ->
        {ok, Reply :: json_term()} |
        {close, Reply :: json_term()} |
        bad_request | internal_server_error |
        term().

The parameters passed to this function are described in the following table:

| Parameter | Type | Description |
|-----------|------|-------------|
| `Method` | `httpd:method()` | The HTTP method or verb (e.g., get, post, delete, etc) used in the request |
| `PathSuffix`| `httpd:path()` | The part of the URL path after the path prefix specified in the HTTP server configuration entry for this handler instance. |
| `HttpRequest`| `httpd:http_request()` | The HTTP request object containing all of the available contextual information about the request. |
| `Args`| `term()` | The arguments passed into the configuration for this handler instance (see below). |

The return value may be one of the following:

| Reply | Description |
|-------|-------------|
| `{ok, Reply :: json_term()}` | Keep the connection open and reply to the HTTP client with a term that can be converted to JSON.  See the JSON Terms section, below. |
| `{close, Reply :: json_term()}` | Close the connection and reply to the HTTP client with a term that can be converted to JSON.  See the JSON Terms section, below. |
| `bad_request` | Close the connection and reply to the server with a BAD_REQUEST (400) error. |
| `not_found` | Close the connection and reply to the server with a NOT_FOUND (404) error. |
| `internal_server_error` | Close the connection and reply to the server with a INTERNAL_SERVER_ERROR (500) error. |
| `term()` | Close the connection and reply to the server with a INTERNAL_SERVER_ERROR (500) error. |

For example, the following implementation services requests for system information if the URL path suffix (after the URL path prefix) is `/system_info`, memory information if the URL path suffix (after the URL path prefix) is `/memory`, and raises an error, otherwise.

    -behavior(http_api_handler).
    -export([..., handle_api_request/4, ...]).

    handle_api_request(get, [<<"system_info">>], HttpRequest, _Args) ->
        Socket = maps:get(socket, HttpRequest),
        {ok, #{addr := Host, port := Port}} = socket:peername(Socket),
        io:format("GET system_info request from ~p:~p~n", [Host, Port]),
        {close, #{
            platform => atomvm:platform(),
            word_size => erlang:system_info(wordsize),
            system_architecture => erlang:system_info(system_architecture),
            atomvm_version => erlang:system_info(atomvm_version),
            esp32_chip_info => get_esp32_chip_info(),
            esp_idf_version => list_to_binary(erlang:system_info(esp_idf_version))
        }};
    handle_api_request(get, [<<"memory">>], HttpRequest, _Args) ->
        Socket = maps:get(socket, HttpRequest),
        {ok, #{addr := Host, port := Port}} = socket:peername(Socket),
        io:format("GET memory request from ~p:~p~n", [Host, Port]),
        {close, #{
            atom_count => erlang:system_info(atom_count),
            process_count => erlang:system_info(process_count),
            port_count => erlang:system_info(port_count),
            esp32_free_heap_size => erlang:system_info(esp32_free_heap_size),
            esp32_largest_free_block => erlang:system_info(esp32_largest_free_block),
            esp32_minimum_free_size => erlang:system_info(esp32_minimum_free_size)
        }};
    handle_api_request(Method, Path, _HttpRequest, _Args) ->
        io:format("ERROR!  Unsupported method ~p or path ~p~n", [Method, Path]),
        bad_request.

#### JSON terms

The type returned in the Reply field of the `handle_api_request` function is a JSON term.  In Erlang/Elixir, this means that the reply must be a map from atoms to atoms, the atoms `true` or `false`, integers, strings, or lists or maps of (recursively defined) JSON terms.

For the most part this type specification should be intuitive, but some examples might be helpful.  Note that Erlang tuples do not have a natural mapping into JSON:

| Erlang Term | JSON mapping |
|-------------|--------------|
| `true` | `true` |
| `false` | `false` |
| `foo` | `"foo"` |
| `"foo"` | `"foo"` |
| `"true"` | `"true"` |
| `<<"foo">>` | `"foo"` |
| `1234` | `1234` |
| `[foo, "bar"]` | `["foo", "bar"]` |
| `#{foo => bar}` | `{"foo": "bar"}` |
| `#{foo => [bar, #{gnu => true}]}` | `{"foo": ["bar", {"gnu": true}]}` |
| `{a, b, c}` | INVALID! |

#### Configuration

The HTML API handler is configured through a HTTP handler configuration entry in the HTTPd server, as described above.  This configuration contains a map with the following elements:

| Key | Type | Value |
|-----|------|-------|
| `handler` | `module()` | `http_api_handler` |
| `handler_config` | `map()` | See below. |

To use the HTML API handler, specify the `http_api_handler` in the `handler` configuration in the HTTPd server configuration for your HTTPd server instance.

The `handler_config` map contains the following elements:

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `module` | `module()` | yes | The Erlang or Elixir module that implements the `http_api_handler` behavior.  (See above) |
| `args` | `term()` | no | An optional set of arguments to pass to the handler.  The value of this entry may be any term and will be passed in the Args parameter to the `handle_api_request` function.  If this value is not defined in configuration, then the handler will be passed the `undefined` atom. |

For example:

        {[<<"api">>]], #{
            handler => http_api_handler,
            handler_config => #{
                module => ?MODULE
            }
        }},

With this example configuration, an HTTP request with the path `/api/system_info` will yield a JSON document with the results of the API request handler above.

> Note.  In accordance with the design of HTTP, the HTTP API Handler and implementations of the `http_api_handler` behavior are inherently stateless.  If you need to maintain state in your handler, then you will need to interact with a stateful service, such as a separately running Erlang process.  This process may be passed through the `args` configuration parameter, or you may use the process registry to locate the stateful process by name.

### The HTTP WebSocket Handler

The HTTP WebSocket Handler is used to allow application developers to implement server-side web sockets in their applications, allowing applications to send and receive data over a dedicated TCP/IP socket between the server and client application (such as a web browser).  Application must implement their own callback modules to use this handler, but by doing so, much of the complexity of writing WebSockets is abstracted away, allowing developers to focus on the application logic, instead of the details of the HTTP protocol.

The HTTP WebSocket Handler API is designed to be as flexible as possible, allowing communication patterns such as request-response and one-way messages (client-to-server, server-to-client).

The API is also designed to hide the low-level websocket protocol, including the websocket handshake, as well as framing and un-framing of data (with and without masking), allowing application developers to focus on the business logic of their applications.

This implementation conforms to version 13 of the [Web Socket](https://datatracker.ietf.org/doc/html/rfc6455) protocol, with the following exceptions:

* No support for sub-protocol negotiation;
* No support for message fragmentation;
* No support for control frames (ping, pong, close, etc).

#### The `http_ws_handler` Behavior

In order to make use of the HTTP Websocket Handler, applications must implement the `http_ws_handler` behavior.  This behavior is implemented via the following callback functions:

    -callback handle_ws_init(WebSocket :: http_ws_handler:websocket(), Path :: httpd:path(), Args :: term()) ->
        {ok, State :: term()} |
        term().

    -callback handle_ws_message(PayloadData :: binary(), State :: term()) ->
        {reply, Reply :: iolist(), NewState :: term()} |
        {noreply, NewState :: term()} |
        {close, Reply :: iolist(), NewState :: term()} |
        {close, NewState :: term()} |
        term().

The `handle_ws_init` function is called during the WebSocket handshake, when a web socket connection is created (typically by the web client).  It should return a state object (a term under the design of the implementor), which is then managed by the underlying HTTP WebSocket handler.  Returning any term other that `{ok, State}` will result in termination of the web socket connection.

The parameters passed to the `handle_ws_init` function are described in the following table:

| Parameter | Type | Description |
|-----------|------|-------------|
| `WebSocket` | `http_ws_handler:websocket()` | A reference to a web socket that can be used to send messages. |
| `Path`| `httpd:path()` | The part of the URL path after the path prefix specified in the HTTP server configuration entry for this handler instance. |
| `Args` | `term()` | The arguments passed into the configuration for this handler instance (see below). |

The `handle_ws_message` is called whenever a message is sent from the client to the HTTPd server over the web socket protocol.  The payload data binary contains the payload passed, together with the current state of the handler (e.g., the state term returned from `handle_ws_init`).

Implementors may optionally return a reply (as an Erlang I/O list), or may, at their discretion, close their end of the socket.

Note that the underlying `http_ws_handler` implementation will manage the framing and un-framing of payload data, per the [Web Socket](https://datatracker.ietf.org/doc/html/rfc6455) protocol.  Users only see the unframed data in this function.

The parameters passed to the `handle_ws_message` function are described in the following table:

| Parameter | Type | Description |
|-----------|------|-------------|
| `PayloadData` | `binary()` | The unframed data passed in the payload from the websocket client to the websocket server.  The format of this data is implemenation-defined. |
| `State` | `term()` | The state term returned from `handle_ws_init` or previous calls to `handle_ws_message`. |

The return value may be one of the following:

| Reply | Description |
|-------|-------------|
| `{reply, Reply :: io_list(), NewState :: term()}` | Reply to the WebSocket client with a blob of data.  The format of this data is implementation-defined. |
| `{noreply, NewState :: term()}` | Keep the connection open and (possibly) update the state. |
| `{close, Reply :: io_list(), NewState :: term()}` | Reply to the websocket client with a blob of data and close the websocket connection. |
| `{close, NewState :: term()}` | Silently close the server-side of the websocket connection. |
| `term()` | Any other return value will silently close the server-side of the websocket connection. |

#### Sending Data Asynchronously

A common pattern in websocket programming is for the server to send data to the client at some interval, or perhaps in response to an event that occurs in the server.

To support this pattern, the `http_ws_handler` module provides the `http_ws_handler:send/2` function.  This function can be used to send a message back to the client at the discretion of the server application.

> Note.  Implementors should NOT use the `http_ws_handler:send/2` function to return data to the sender from within the execution context of the `handle_ws_message` function.  If you wish to send a reply to the sender in this context, use the `reply` return value (see below).

The parameters to the `http_ws_handler:send/2` function are as follows:

| Parameter | Type | Description |
|-----------|------|-------------|
| `WebSocket` | `http_ws_handler:websocket()` | A reference to a web socket that can be used to send messages. |
| `PayloadData` | `binary()` | The unframed data to send to the websocket client to the websocket server.  The format of this data is implemenation-defined. |

#### Configuration

The HTML WebSocket handler is configured through a HTTP handler configuration entry in the HTTPd server, as described above.  This configuration contains a map with the following elements:

| Key | Type | Value |
|-----|------|-------|
| `handler` | `module()` | `http_ws_handler` |
| `handler_config` | `map()` | See below. |

To use the HTML API handler, specify the `http_ws_handler` in the `handler` configuration in the HTTPd server configuration for your HTTPd server instance.

The `handler_config` map contains the following elements:

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `module` | `module()` | yes | The Erlang or Elixir module that implements the `http_ws_handler` behavior.  (See above) |
| `args` | `term()` | no | An optional set of arguments to pass to the handler.  The value of this entry may be any term and will be passed in the Args parameter to the `handle_ws_init` function.  If this value is not defined in configuration, then the handler will be passed the `undefined` atom. |

For example:

        {[<<"ws">>]], #{
            handler => http_ws_handler,
            handler_config => #{
                module => ?MODULE
            }
        }},

With this example configuration, an HTTP request with the path prefix `/ws/` will trigger invocations of the `handle_ws_init/3` and `handle_ws_message/2` callback functions in the current module.

For example, the following implementation will spawn a thread that sends information about the heap size and other information back to the websocket client every 5 seconds.  It will also handle messages that are sent to it from the websocket client.  If the client sends a message with `ping` in the payload, then the server will respond with `pong`.  Otherwise, it will simply print the received message to the console.

    -behavior(http_ws_handler).
    -export([..., handle_ws_init/3, handle_ws_message/2, ...]).

    handle_ws_init(WebSocket, _Path, _Args) ->
        spawn(fun() -> update_loop(WebSocket) end),
        {ok, undefined}.

    handle_ws_message(<<"ping">>, State) ->
        {reply, <<"pong">>, State};
    handle_ws_message(Message, State) ->
        io:format("Received message from web socket.  Message: ~p~n", [Message]),
        {noreply, State}.

    update_loop(WebSocket) ->
        timer:sleep(5000),
        Binary = json_encoder:encode(maps:to_list(#{
            esp32_free_heap_size => erlang:system_info(esp32_free_heap_size),
            esp32_largest_free_block => erlang:system_info(esp32_largest_free_block),
            esp32_minimum_free_size => erlang:system_info(esp32_minimum_free_size)
        })),
        io:format("Sending message to client ~p ...~n", [Binary]),
        http_ws_handler:send(WebSocket, Binary),
        update_loop(WebSocket).


### The HTTP Handler API

The generic HTTP Handler API is a lower-level API which the HTTP File handler, HTTP API handler, and HTTP WebSocket handler all use to implement their functionality.  Most users will not have a need to use this API, so the documentation is light on this particular API.  Consult the implementation for more information about this API.

TODO

    -callback handle_http_req(Method :: atom(), PathSuffix :: list(), HttpRequest :: term(), HandlerConfig :: map()) ->
        ok |
        {ok, Reply :: term()} |
        {ok, ContentType :: string(), Reply :: term()} |
        not_found |
        bad_request |
        internal_server_error |
        term().


# `httpd_example` Example

The `httpd_example` example program illustrates a simple web server.  See the [README](../examples/httpd_example/README.md) for information about how to build, flash, and run this example program.
