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
%% @spec
%% @end
%%--------------------------------------------------------------------
return_record() ->
    gen_server:call(?SERVER, {return, all}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
return_record(ID)->
    gen_server:call(?SERVER, {return, {id, ID}}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
fetch(google, RequestID) ->
    gen_server:cast(?SERVER, {fetch, google, RequestID});
fetch(sumup, RequestID) ->
    gen_server:cast(?SERVER, {fetch, sumup, RequestID}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
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
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    inets:start(),
    TableID = ets:new(request_records, []),    
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
    Reply = ets:match(State#state.table_id, {'$1', '_', '_', '$2'}),
    {reply, Reply, State};

handle_call({return, {id, ID}}, _From, State) ->
    Reply = case ets:lookup(State#state.table_id, ID) of
		[{Id, _, _, Time}] ->
		    [Id, Time];
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
    ets:insert(State#state.table_id, {RequestID, create_timestamp()}),
    spawn(fun() -> do_get(google, RequestID) end),
    {noreply, State};
handle_cast({fetch, sumup, RequestID}, State) ->
    ets:insert(State#state.table_id, {RequestID, create_timestamp()}),
    spawn(fun() -> do_get(sumup, RequestID) end),
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
handle_info({do_get, successfull, ReqId, FinishTime}, State) ->
    [{ReqId, StartTime}] = ets:lookup(State#state.table_id, ReqId),
    ElapsedTime = FinishTime - StartTime,
    ets:insert(State#state.table_id, {ReqId, StartTime, FinishTime, ElapsedTime}), 
    {noreply, State};
handle_info({do_get, failed_connect, ReqId, FinishTime}, State) ->
    [{ReqId, StartTime}] = ets:lookup(State#state.table_id, ReqId),
    ElapsedTime = FinishTime - StartTime,
    ets:insert(State#state.table_id, {ReqId, StartTime, FinishTime, ElapsedTime}), 
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
    inets:stop(),
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
%% @spec
%% @end
%%--------------------------------------------------------------------
do_get(Site, ReqId) ->
    {Protocol, URL} = case Site of
			  google -> {httpc, "http://www.google.com"};
			  sumup -> {httpc, "https://sumup.co.uk/"}
		      end,
    Response = Protocol:request(get, {URL, []}, [], []),
    case Response of
	{error, {failed_connect, _}} ->
	    ?SERVER ! {do_get, failed_connect, ReqId, create_timestamp()};
	_ ->
	    ?SERVER ! {do_get, successfull, ReqId, create_timestamp()}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_timestamp() ->    
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) ->
		[element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).
