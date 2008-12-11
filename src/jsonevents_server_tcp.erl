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
    gen_listener_tcp:start({local, ?LISTENER}, ?MODULE, [erlcfg:new()], []).

start_link(Config) ->
    gen_listener_tcp:start_link({local, ?LISTENER}, ?MODULE, [Config], []).

init([Config]) ->
    {ok, 
        {
            Config:get(server.port, 8195), 
            [ 
                binary, 
                inet, 
                {active, false}, 
                {backlog, Config:get(server.backlog, 10)},
                {reuseaddr, true} 
            ], 
            
            { 
                jsonevents_socket_client_sup, 
                start_child, 
                []
            } 
        }
    }.
