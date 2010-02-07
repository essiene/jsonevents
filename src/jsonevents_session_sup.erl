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

-module(jsonevents_session_sup).
-behaviour(supervisor).
-include("jsonevents.hrl").


-export([
        start_link/0,
        start_child/1,
        start_child/6
    ]).

-export([
        init/1
    ]).

start_link() ->
    Res = supervisor:start_link({local, ?SESSION_SUP}, ?MODULE, []),
    error_logger:info_report({?MODULE, started}),
    Res.

start_child(ClientSocket) -> 
    start_child(ClientSocket, inet, gen_tcp, tcp, tcp_closed, tcp_error).

start_child(ClientSocket, Inet, Transport, TagData, TagClosed, TagError) -> 
    error_logger:info_report([?MODULE, starting_child, {inet, Inet}, {transport, Transport}, {tag_data, TagData}, {tag_closed, TagClosed}, {tag_error, TagError}]),
    supervisor:start_child(?SESSION_SUP, [ClientSocket, Inet, Transport, TagData, TagClosed, TagError]).

init([]) ->
    {ok, 
        {
            {simple_one_for_one, 3, 10}, 
            [ 
                {
                    undefined, 
                    {jsonevents_session, start_link, []}, 
                    transient, 
                    5000, 
                    worker, 
                    [jsonevents_session]
                }
            ]
        }
    }.
