-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    {ok, _Apps} = application:ensure_all_started(myapp).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", myapp_default_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8001}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    myapp_sup:start_link().

stop(_State) ->
    ok.
