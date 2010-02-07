%% 
%% Copyright (c) 2008-2010, Essien Ita Essien
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted 
%% provided that the following conditions are met:
%%
%%    * Redistributions of source code must retain the 
%%      above copyright notice, this list of conditions 
%%      and the following disclaimer.
%%    * Redistributions in binary form must reproduce 
%%      the above copyright notice, this list of 
%%      conditions and the following disclaimer in the 
%%      documentation and/or other materials provided with 
%%      the distribution.
%%    * Neither the name "JsonEvents" nor the names of its 
%%      contributors may be used to endorse or promote 
%%      products derived from this software without 
%%      specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
%% OF SUCH DAMAGE.
%% 

-module(jsonevents_bus).
-behaviour(gen_server).
-include("jsonevents.hrl").

-export([
        broadcast/2,
        connect/0,
        disconnect/0
    ]).

-export([
        start/0,
        start_link/0,
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
    gen_server:start({local, ?EVENT_BUS}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?EVENT_BUS}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?EVENT_BUS, stop).



broadcast(Name, Body) ->
    ErlJson = to_erljson([{event, Name} | Body]),
    Json = mochijson2:encode(ErlJson),
    gen_server:cast(?EVENT_BUS, {broadcast, Json ++ "\r\n"}).

connect() ->
    gen_server:call(?EVENT_BUS, {connect, self()}).

disconnect() ->
    gen_server:call(?EVENT_BUS, {disconnect, self()}).


init([]) ->
    Pids = [],

    error_logger:info_report({event_bus, starting}),
    {ok, Pids}.

handle_call({connect, Pid}, _From, Pids) ->
    error_logger:info_report([new_event_bus_connection, {from, Pid}]),

    case lists:member(Pid, Pids) of
        true ->
            {reply, ok, Pids};
        false ->
            NewPids = [Pid | Pids],
            {reply, ok, NewPids}
    end;

handle_call({disconnect, Pid}, _From, Pids) ->
    error_logger:info_report([event_bus_disconnection, {from, Pid}]),

    NewPids = lists:delete(Pid, Pids),
    {reply, ok, NewPids};

handle_call(Request, _From, Pids) ->
    {reply, {illegal, Request}, Pids}.

handle_cast({broadcast, JsonString}, Pids) ->
    Fun = fun(Pid) ->
            gen_fsm:send_event(Pid, {?EVENT_BUS, {broadcast, JsonString}})
    end,

    lists:foreach(Fun, Pids),
    {noreply, Pids};

handle_cast(stop, Pids) ->
    {stop, normal, Pids};

handle_cast(_Request, Pids) ->
    {noreply, Pids}.

handle_info(_Request, Pids) ->
    {noreply, Pids}.

terminate(_Reason, _Pids) ->
    ok.

code_change(_OldVsn, Pids, _Extra) ->
    {noreply, Pids}.



to_erljson(TupleList) ->
    to_erljson(TupleList, []).

to_erljson([], Accm) ->
    NAccm = lists:reverse(Accm),
    {struct, NAccm};

to_erljson([{Key, Val} | Rest], Accm) when is_atom(Key), is_atom(Val) ->
    NKey = list_to_binary(atom_to_list(Key)),
    NVal = list_to_binary(atom_to_list(Val)),
    to_erljson(Rest, [{NKey, NVal} | Accm]);

to_erljson([{Key, Val} | Rest], Accm) when is_atom(Key), is_list(Val) ->
    NKey = list_to_binary(atom_to_list(Key)),
    NVal = list_to_binary(Val),
    to_erljson(Rest, [{NKey, NVal} | Accm]);

to_erljson([{Key, Val} | Rest], Accm) when is_atom(Key), is_number(Val) ->
    NKey = list_to_binary(atom_to_list(Key)),
    to_erljson(Rest, [{NKey, Val} | Accm]);

to_erljson([{Key, Val} | Rest], Accm) when is_atom(Key), is_boolean(Val) ->
    NKey = list_to_binary(atom_to_list(Key)),
    to_erljson(Rest, [{NKey, Val} | Accm]).
