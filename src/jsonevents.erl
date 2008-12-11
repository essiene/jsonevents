-module(jsonevents).
-include("jsonevents.hrl").

-export([
        start_server/1,
        stop_server/1
    ]).


start_server(ConfigFile) ->
    jsonevents_sup:start_server_link(ConfigFile).

stop_server(Pid) ->
    exit(Pid, normal).
