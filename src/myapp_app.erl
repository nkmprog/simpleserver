-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    {ok, _} = application:ensure_all_started(myapp).

    %% ok = application:start(asn1),
    %% ok = application:start(crypto),
    %% ok = application:start(public_key),
    %% ok = application:start(ssl),
    %% ok = application:start(ranch),
    %% ok = application:start(cowboy),
    %% ok = application:start(myapp).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", myapp_default_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    myapp_sup:start_link().

stop(_State) ->
    ok.
