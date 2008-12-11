-module(jsonevents_sup).
-behaviour(supervisor).
-include("jsonevents.hrl").

-export([
        start_link/1,
        start_server_link/1,
        stop/0
    ]).

-export([
        init/1
    ]).

start_link(ConfigFile) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [ConfigFile]).

start_server_link(ConfigFile) ->
    supervisor:start_link(?MODULE, [ConfigFile]).

stop() ->
    exit(?SUPERVISOR, normal).


init([ConfigFile]) ->
    Config = case erlcfg:new(ConfigFile) of
        {error, Reason} ->
            throw(Reason);
        Cfg ->
            Cfg
    end,

    ClientSup = {?SOCKET_CLIENT_SUP, 
        {jsonevents_socket_client_sup, start_link, []}, 
        permanent, 5000, supervisor, [jsonevents_socket_client_sup]
    },

    Listener = {?LISTENER, 
        {jsonevents_server_tcp, start_link, [Config]}, 
        permanent, 5000, worker, [jsonevents_server_tcp] 
    },

    {
        ok, 
        {
            {one_for_one, 3, 10}, 
            [ClientSup, Listener]
        }
    }.
