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
-export([start_link/0, return_record/0, return_record/1, fetch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).

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
fetch(google) ->
    gen_server:call(?SERVER, {fetch, google});
fetch(sumup) ->
    gen_server:call(?SERVER, {fetch, sumup}).

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
    {ok, #state{}}.

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
    Reply = ok,
    {reply, Reply, State};
handle_call({return, {id, _ID}}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};    
handle_call({fetch, google}, _From, State) ->
    Reply = do_get(google),
    {reply, Reply, State};
handle_call({fetch, sumup}, _From, State) ->
    Reply = do_get(sumup),
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
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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
do_get(Site) ->
    {Protocol, URL} = case Site of
			  google -> {httpc, "http://www.google.com"};
			  sumup -> {httpc, "https://sumup.co.uk/"}
		      end,
    Response = Protocol:request(get, {URL, []}, [], []),
    case Response of
	{error, {failed_connect, _}} ->
	    {error, failed_connect};
	_ ->
	    Response
    end.
