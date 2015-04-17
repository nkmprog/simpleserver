-module(myapp_default_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path_info(Req),
    {ok, Req2} = handle(Method, Path, Req),
    {ok, Req2, State}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle(<<"GET">>, [<<"stats">>], Req) ->
    Params = cowboy_req:parse_qs(Req),
    {ok, RequestID} = proplists:get_value(<<"id">>, Params),
    case RequestID of
	undefined ->
	    %% Issue a call to the backend server to return all records
	    mybackend:return_record();
	ID ->
	    %% Issue a call to the backend server to return a
	    %% specific record
	    mybackend:return_record(ID)
    end;
handle(<<"POST">>, [<<"communication">>], Req) ->
    {ok, Data, _Req2} = cowboy_req:body(Req, []),
    {DecodedData} = jiffy:decode(Data),
    {ok, Provider} = proplists:get_value(<<"provider">>, DecodedData),    
    case Provider of
	<<"google">> ->
	    %% issue a call to fetch google.com
	    mybackend:fetch(google);
	<<"sumup">> ->
	    %% issue a call to fetch sumup.com
	    mybackend:fetch(sumup)
    end;
handle(_, _, Req) ->
    cowboy_req:reply(404, Req).
