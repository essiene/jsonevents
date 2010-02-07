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

-module(jsonevents_session).
-behaviour(gen_fsm).
-include("jsonevents.hrl").

-export([
        start/6,
        start_link/6
    ]).

-export([
        active/2,
        active/3
    ]).


-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).


start(ClientSocket, Inet, Transport, TagData, TagClosed, TagError) ->
    gen_fsm:start(?MODULE, [ClientSocket, Inet, Transport, TagData, TagClosed, TagError], []).

start_link(ClientSocket, Inet, Transport, TagData, TagClosed, TagError) ->
    error_logger:info_msg("Starting session~n"),
    gen_fsm:start_link(?MODULE, [ClientSocket, Inet, Transport, TagData, TagClosed, TagError], []).


active({?EVENT_BUS, {broadcast, Data}}, #session_state{sock=ClientSocket, transport=Transport}=St) ->
    error_logger:info_report([relaying_broadcast, {data, Data}, {csock, ClientSocket}, {pid, self()}]),
    case Transport:send(ClientSocket, Data) of
        ok -> 
            {next_state, active, St};
        {error, Reason} ->
            error_logger:error_report([?MODULE, {state, active}, {error, Reason}]),
            {stop, normal, St}
    end;

active(_Event, St) ->
    {next_state, active, St}.

active(Event, _From, St) ->
    {reply, {illegal, Event}, active, St}.


init([ClientSocket, Inet, Transport, TagData, TagClosed, TagError]) ->
    ok = Inet:setopts(ClientSocket, [{active, once}]),

    error_logger:info_report([connecting_to_event_bus, {csock, ClientSocket}, {pid, self()}]),
    jsonevents_bus:connect(),

    State = #session_state{
        sock=ClientSocket,
        inet=Inet,
        transport=Transport,
        tag_data=TagData,
        tag_closed=TagClosed,
        tag_error=TagError
    },

    {ok, active, State}.

handle_event(_Event, State, St) ->
    {next_state, State, St}.

handle_sync_event(Event, _From, State, St) ->
    {reply, {illegal, Event}, State, St}.

handle_info({TagData, ClientSocket, _Binary}, State, #session_state{sock=ClientSocket, tag_data=TagData, inet=Inet}=St) ->
    ok = Inet:setopts(ClientSocket, [{active, once}]),
    {next_state, State, St};

handle_info({TagError, ClientSocket, Reason}, _State, #session_state{sock=ClientSocket, tag_error=TagError}=St) ->
    {stop, Reason, St};

handle_info({TagClosed, ClientSocket}, _State, #session_state{sock=ClientSocket, tag_closed=TagClosed}=St) ->
    {stop, normal, St};

handle_info(_Info, State, St) ->
    {next_state, State, St}.

terminate(_Reason, _State, #session_state{sock=ClientSocket, transport=Transport}) ->
    error_logger:info_report([disconnecting_from_event_bus, {csock, ClientSocket}, {pid, self()}]),
    jsonevents_bus:disconnect(),

    error_logger:info_report([connection_closing, {csock, ClientSocket}, {pid, self()}]),
    Transport:close(ClientSocket),
    ok.

code_change(_OldVsn, State, St, _Extra) ->
    {next_state, State, St}.
