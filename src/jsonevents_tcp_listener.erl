-module(jsonevents_tcp_listener).
-include("jsonevents.hrl").
-behaviour(je_tcp_listener).

-export([
        start/0,
        start_link/1
    ]).

-export([
        init/1
    ]).

start() ->
    je_tcp_listener:start({local, ?LISTENER}, ?MODULE, [8595], []).

start_link(Port) when is_number(Port) ->
    je_tcp_listener:start_link({local, ?LISTENER}, ?MODULE, [Port], []);

start_link(ClientSocket) when is_port(ClientSocket) ->
    error_logger:info_report({new_connection, ClientSocket}).



init([Port]) ->
    {
        ok, 
        Port,
        [
            binary, 
            inet, 
            {active, false}, 
            {backlog, 10},
            {reuseaddr, true}
        ],
        {
            ?MODULE,
            start_link,
            []
        }
    }.
