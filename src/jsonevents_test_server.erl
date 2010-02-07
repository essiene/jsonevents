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
