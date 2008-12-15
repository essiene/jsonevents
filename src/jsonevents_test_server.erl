-module(jsonevents_test_server).
-behaviour(gen_server).

-export([
        start/0,
        start/1,
        stop/0
    ]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).



start() ->
    start("/etc/jsonevents.conf").

start(ConfigFile) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [ConfigFile], []).

stop() ->
    gen_server:cast(?MODULE, stop).



init([ConfigFile]) ->
    timer:send_interval(10000, {?MODULE, push}),
    jsonevents:start(ConfigFile).


handle_call(Request, _From, StateData) ->
    {reply, {illegal, Request}, StateData}.


handle_cast(stop, Pid=StateData) ->
    jsonevents:stop(Pid),
    {stop, normal, StateData};

handle_cast(_Request, StateData) ->
    {noreply, StateData}.

handle_info({?MODULE, push}, StateData) ->
    {{YYYY, MM, DD}, {HH, Mm, Ss}} = erlang:localtime(),
    jsonevents:broadcast(current_time, [ 
            {year, YYYY},
            {month, MM},
            {day, DD},
            {hour, HH},
            {minute, Mm},
            {second, Ss}
        ]),
    {noreply, StateData};

handle_info(_Request, StateData) ->
    {noreply, StateData}.

terminate(_Reason, _StateData) ->
    ok.

code_change(_OldVsn, StateData, _Extra) ->
    {noreply, StateData}.
