% Code for the individual processes that store the cached data
-module(sc_element).

-behaviour(gen_server).

% custom callbacks
-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).

% otp gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60*60*24)).

-record(state, {value, lease_time, start_time}).

% Module API
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    sc_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

% Modules gen_server callbacks

init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok,
        #state{value = Value,
              lease_time = LeaseTime,
              start_time = StartTime},
        time_left(StartTime, LeaseTime)}.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000 % convert to milliseconds
    end.

handle_call(fetch, _From, State) ->
    #state{value = Value, lease_time = LeaseTime, start_time = StartTime} =
        State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime,
           start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    % receives a timeout message, and after stop terminate is called 
    {stop, normal, State}.

% terminate gives us a chance to clean things up, e.g.
% remove all traces of something like key->Pid in our store
terminate(_Reason, _State) ->
    % this sort of shows two things that the authors thinks is OK:
    % 1. Something low in the hierarchy calls up to a controller of sorts
    % 2. Instead of using supervisor:which_children/x we use our own custom
    %    server to keep track of some key => Pid mapping.
    %
    % What i wonder with this architecture:
    % If a call to sc_store:delete(self()), should be executed 
    % right after a fetch to the sc_store(), wouldn't we allow someone to
    % call a killed process in the faith that it exist?
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
