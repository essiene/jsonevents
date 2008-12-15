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
    Config = get_config_or_die(ConfigFile),
    start(Config);

start(Config) ->
    supervisor:start_link(?MODULE, [Config]).

start_link(ConfigFile) when is_list(ConfigFile) ->
    Config = get_config_or_die(ConfigFile),
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

    EventBus = {?EVENT_BUS,
        {jsonevents_bus, start_link, []},
        permanent, 5000, worker, [jsonevents_bus]
    },

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
            [EventBus, ClientSup, Listener]
        }
    }.


get_config_or_die(ConfigFile) ->
    case erlcfg:new(ConfigFile) of
        {error, Reason} ->
            throw(Reason);
        Cfg ->
            Cfg
    end.

