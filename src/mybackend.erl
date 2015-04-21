%%%-------------------------------------------------------------------
%%% @author bobi <bobi@bobi-Satellite-L755>
%%% @copyright (C) 2015, bobi
%%% @doc
%%%
%%% @end
%%% Created : 17 Apr 2015 by bobi <bobi@bobi-Satellite-L755>
%%%-------------------------------------------------------------------
-module(mybackend).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 return_record/0,
	 return_record/1,
	 fetch/2,
	 create_unique_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {table_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc 
%% Returns all request records.
%% 
%% @spec return_record() -> Reply
%% where
%%       Reply = [RecordInfo1, ..., RecordInfoN],
%%       RecordInfo = [FinishTime, RequestId, Provider,
%%                                 ResponseTime, TotalTime],
%%       FinishTime = int(),
%%       RequestId = string(),
%%       Provider = google | youtube,
%%       ResponseTime = int(),
%%       TotalTime = int()
%% @end 
%%--------------------------------------------------------------------
return_record() ->
    gen_server:call(?SERVER, {return, all}).

%%--------------------------------------------------------------------
%% @doc
%% Return a specific request record.
%% 
%% @spec return_record() -> Reply
%% where
%%       Reply = [RecordInfo],
%%       RecordInfo = [FinishTime, RequestId, Provider,
%%                                 ResponseTime, TotalTime],
%%       FinishTime = int(),
%%       RequestId = string(),
%%       Provider = google | youtube,
%%       ResponseTime = int(),
%%       TotalTime = int()
%% @end
%%--------------------------------------------------------------------
return_record(ID)->
    gen_server:call(?SERVER, {return, {id, ID}}).

%%--------------------------------------------------------------------
%% @doc
%% Sends an asynchronous request and returns ok immediately
%%
%% @spec fetch(Provider, RequestID) -> ok
%% where 
%%       Provider = google | youtube,
%%       RequestID = string()
%% @end
%%--------------------------------------------------------------------
fetch(google, RequestID) ->
    gen_server:cast(?SERVER, {fetch, google, RequestID});
fetch(youtube, RequestID) ->
    gen_server:cast(?SERVER, {fetch, youtube, RequestID}).

%%--------------------------------------------------------------------
%% @doc
%% Makes a synchronous request to the gen_server to create a unique id
%%
%% @spec create_unique_id() -> string()
%% @end
%%--------------------------------------------------------------------
create_unique_id() ->
    gen_server:call(?SERVER, {create, id}).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server state. Creates a new ets table which holds
%% record information.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    TableID = ets:new(request_records, [ordered_set]),    
    {ok, #state{table_id=TableID}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({return, all}, _From, State) ->
    Reply = ets:match(State#state.table_id, {'$1', '$2', '$3', '$4', '$5'}),
    {reply, Reply, State};
handle_call({return, {id, ReqId}}, _From, State) ->
    Reply = case ets:match(State#state.table_id, {'$1', ReqId, '$3', '$4', '$5'}) of
		[[Id, Provider, ResponseTime, TotalTime]] ->
		    [Id, ReqId, Provider, ResponseTime, TotalTime];
		_ ->
		    undefined
	    end,
    {reply, Reply, State};
handle_call({create, id}, _From, State) ->
    Timestamp = integer_to_list(create_timestamp()),
    AlphaNumString = random_string(16),
    Reply = Timestamp ++ AlphaNumString,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({fetch, google, RequestID}, State) ->
    ets:insert(State#state.table_id, {RequestID, google, create_timestamp(micros)}),
    spawn(fun() -> do_get(google, RequestID) end),
    {noreply, State};
handle_cast({fetch, youtube, RequestID}, State) ->
    ets:insert(State#state.table_id, {RequestID, youtube, create_timestamp(micros)}),
    spawn(fun() -> do_get(youtube, RequestID) end),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({do_get, successfull, ReqId, ResponseTime}, State) ->
    [{ReqId, Provider, StartTime}] = ets:lookup(State#state.table_id, ReqId),
    TotalTime = create_timestamp(micros) - StartTime,
    ets:insert(State#state.table_id, {create_timestamp(micros),
				      ReqId, Provider, ResponseTime, TotalTime}), 
    {noreply, State};
handle_info({do_get, failed_connect, ReqId, ResponseTime}, State) ->
    [{ReqId, Provider, StartTime}] = ets:lookup(State#state.table_id, ReqId),
    TotalTime = create_timestamp(micros) - StartTime,
    ets:insert(State#state.table_id, {create_timestamp(micros),
				      ReqId, Provider, ResponseTime, TotalTime}), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ets:delete(State#state.table_id),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Makes a sychronous http request to either google or youtube.
%% 
%% @spec do_get(Site, RequestID) -> 
%%                  {do_get, failed_connect, RequestID, ElapsedTime} |
%%                  {do_get, successfull, RequestID, ElapsedTime}
%% where
%%       Site = google | youtube,
%%       RequestID = string(),
%%       ElapsedTime = int()
%%
%% @end
%%--------------------------------------------------------------------
do_get(Site, ReqId) ->
    {Protocol, URL} = case Site of
			  google -> {httpc, "http://www.google.com"};
			  youtube -> {httpc, "http://www.youtube.com"}
		      end,
    Start = create_timestamp(micros),
    Response = Protocol:request(get, {URL, []}, [], []),
    Elapsed = create_timestamp(micros) - Start,

    case Response of
	{error, {failed_connect, _}} ->
	    ?SERVER ! {do_get, failed_connect, ReqId, Elapsed};
	_ ->
	    ?SERVER ! {do_get, successfull, ReqId, Elapsed}
    end.

%%--------------------------------------------------------------------
%% Creates a timestamp in either seconds, milliseconds or microseconds
%%--------------------------------------------------------------------
create_timestamp() ->    
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

create_timestamp(millis) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000 + Seconds) * 1000 + (MicroSeconds div 1000);

create_timestamp(micros) ->
    {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.

%%--------------------------------------------------------------------
%% Creates a random string
%%--------------------------------------------------------------------
random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) ->
		[element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).
