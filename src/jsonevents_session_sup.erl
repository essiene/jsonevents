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
