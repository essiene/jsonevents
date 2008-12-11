-module(jsonevents_tcp_listener).
-include("jsonevents.hrl").

-export([
        start_link/1
    ]).

-export([
        init/1
    ]).

start_link(Port) ->
    je_tcp_listener:start_link({local, ?LISTENER}, ?MODULE, [Port], []).



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
        ]
    }.
