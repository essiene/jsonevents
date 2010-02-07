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

-module(jsonevents).
-behaviour(supervisor).
-include("jsonevents.hrl").

-export([broadcast/2]).

-export([
        start/1,
        start_link/1,
        stop/0,
        stop/1
    ]).

-export([
        init/1
    ]).



start(ConfigFile) when is_list(ConfigFile) ->
    {ok, Config} = erlcfg:new(ConfigFile, true),
    start(Config);

start(Config) ->
    supervisor:start_link(?MODULE, [Config]).

start_link(ConfigFile) when is_list(ConfigFile) ->
    {ok, Config} = erlcfg:new(ConfigFile, true),
    start_link(Config);

start_link(Config) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [Config]).

stop() ->
    exit(?SUPERVISOR, normal).

stop(Pid) ->
    exit(Pid, normal).

broadcast(Name, Body) ->
    jsonevents_bus:broadcast(Name, Body).

init([Config]) ->
    application:start(crypto),
    application:start(ssl),

    EventBus = {?EVENT_BUS,
        {jsonevents_bus, start_link, []},
        permanent, 5000, worker, [jsonevents_bus]
    },

    ClientSup = {?SESSION_SUP, 
        {jsonevents_session_sup, start_link, []}, 
        permanent, 5000, supervisor, [jsonevents_session_sup]
    },

    TcpListener = {?TCP_LISTENER, 
        {jsonevents_server_tcp, start_link, [Config]}, 
        permanent, 5000, worker, [jsonevents_server_tcp] 
    },

    SslListener = {?SSL_LISTENER, 
        {jsonevents_server_ssl, start_link, [Config]}, 
        permanent, 5000, worker, [jsonevents_server_ssl] 
    },

    {
        ok, 
        {
            {one_for_one, 3, 10}, 
            [EventBus, ClientSup, TcpListener, SslListener]
        }
    }.
