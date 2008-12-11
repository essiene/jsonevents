-module(jsonevents_server_tcp).
-include("jsonevents.hrl").
-behaviour(gen_listener_tcp).

-export([
        start/0,
        start_link/1
    ]).

-export([
        init/1
    ]).

start() ->
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [8595], []).

start_link(Port) when is_number(Port) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Port], []).

init([Port]) ->
    {ok, 
        {
            Port, 
            [ 
                binary, 
                inet, 
                {active, false}, 
                {backlog, 10}, 
                {reuseaddr, true} 
            ], 
            
            { 
                jsonevents_socket_client_sup, 
                new_connection, 
                []
            } 
        }
    }.
