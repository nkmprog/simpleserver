-module(myapp_default_handler).

%% The Cowboy HTTP handler is modeled after Erlang/OTP's gen_server
%% behaviour, although simplified, as Cowboy will simply call the
%% three callbacks sequentially.
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
%% Initialize the state for this request.
%% The shutdown return value can be used to skip the handle/2 call
%% entirely.
%%
%% @spec init({TransportName, ProtocolName}, Request, Options) ->
%%         {ok, Request, State} |
%%         {shutdown, Request, State}
%% @end
%%--------------------------------------------------------------------
init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handle the request.
%%
%% This callback is where the request is handled and a response should
%% be sent. If a response is not sent, Cowboy will send a 204 No Content
%% response automatically.
%% @spec handle(Req, State) -> {ok, Req, State}
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path_info(Req),
    {ok, Req2} = handle(Method, Path, Req),
    {ok, Req2, State}.

%%--------------------------------------------------------------------
%% @doc
%% Perform any necessary cleanup of the state.
%%
%% This callback should release any resource currently in use,
%% clear any active timer and reset the process to its original state,
%% as it might be reused for future requests sent on the same connection.
%% Typical plain HTTP handlers rarely need to use it.
%%
%% @spec terminate(Reason, Req, State) -> ok
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% Send a response to the client.
%% 
%% This function effectively sends the response status line,
%% headers and body to the client, in a single send function call.
%%
%% @spec handle(Method, Path, Request) -> cowboy_req:reply/3
%% @end
%%--------------------------------------------------------------------
handle(<<"GET">>, [<<"stats">>], Req) ->
    %% If the client does not specify any request id than statistical
    %% information is retrieved for all requests made so far.

    {Params, _} = cowboy_req:qs_vals(Req),
    RequestID = proplists:get_value(<<"id">>, Params),

    case RequestID of
    	undefined ->
    	    %% Issue a synchronous call to the backend server to return all records
    	    Reqs = mybackend:return_record(),
    	    Data = [{[{req_id, list_to_binary(ReqId)},
		      {provider, Provider},
		      {finish_time, Id},
		      {response_time, ResponseTime},
		      {total_time, TotalTime}]} || 
		       [Id, ReqId, Provider, ResponseTime, TotalTime] <- Reqs],
    	    JSON = jiffy:encode({[{data, Data}]}, [pretty]),
    	    cowboy_req:reply(200, [
    				   {<<"content-type">>, <<"application/json">>}
    				  ], JSON, Req);
    	ID ->
    	    %% Issue a synchronous call to the backend server to return a
    	    %% specific record
    	    case mybackend:return_record(binary_to_list(ID)) of
    		undefined ->
    		    cowboy_req:reply(404, Req);
    		[Id, ReqId, Provider, ResponseTime, TotalTime] ->
    		    JSON = jiffy:encode({[{req_id, list_to_binary(ReqId)},
					  {provider, Provider},
					  {finish_time, Id},
					  {response_time, ResponseTime},
					  {total_time, TotalTime}]}, [pretty]),
    		    cowboy_req:reply(200, [
    					   {<<"content-type">>, <<"application/json">>}
    					  ], JSON, Req)
    	    end
    end;
handle(<<"POST">>, [<<"communication">>], Req) ->
    %% Parse the requested provider from the JSON Data send by the client

    {ok, Data, _Req2} = cowboy_req:body(Req, []),
    {DecodedData} = jiffy:decode(Data),
    Provider = proplists:get_value(<<"provider">>, DecodedData),

    %% Ask the backend server to create a unique id for the current
    %% request. This id will be returned to the client
    ID = mybackend:create_unique_id(),

    %% issue an asynchronous to the backend server to fetch either 
    %% google or youtube
    case Provider of
	<<"google">> ->
	    ok = mybackend:fetch(google, ID);
	<<"youtube">> ->
	    ok = mybackend:fetch(youtube, ID)
    end,

    %% The backend server is processing the request asynchronously -
    %% reply with a (202, Accepted) status code and return the request id
    %% to the client
    JSON = jiffy:encode({[{<<"request_id">>, list_to_binary(ID)}]}),
    cowboy_req:reply(202, [
			   {<<"content-type">>, <<"application/json">>}
			  ], JSON, Req);
handle(_, _, Req) ->
    cowboy_req:reply(404, Req).
