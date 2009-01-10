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
    gen_listener_tcp:start({local, ?TCP_LISTENER}, ?MODULE, [erlcfg:new()], []).

start_link(Config) ->
    gen_listener_tcp:start_link({local, ?TCP_LISTENER}, ?MODULE, [Config], []).

init([Config]) ->
    {ok, 
        {
            Config:get(server.port.tcp, 8195), 
            [ 
                binary, 
                inet, 
                {active, false}, 
                {backlog, Config:get(server.backlog, 10)},
                {reuseaddr, true} 
            ], 
            
            { 
                jsonevents_session_sup, 
                start_child, 
                []
            } 
        }
    }.
