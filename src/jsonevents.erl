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
    application:start(crypto),
    application:start(ssl),

    EventBus = {?EVENT_BUS,
        {jsonevents_bus, start_link, []},
        permanent, 5000, worker, [jsonevents_bus]
    },

    ClientSup = {?SESSION_SUP, 
        {jsonevents_session_sup, start_link, []}, 
        permanent, 5000, supervisor, [jsonevents_session_sup]
    },

    TcpListener = {?TCP_LISTENER, 
        {jsonevents_server_tcp, start_link, [Config]}, 
        permanent, 5000, worker, [jsonevents_server_tcp] 
    },

    SslListener = {?SSL_LISTENER, 
        {jsonevents_server_ssl, start_link, [Config]}, 
        permanent, 5000, worker, [jsonevents_server_ssl] 
    },

    {
        ok, 
        {
            {one_for_one, 3, 10}, 
            [EventBus, ClientSup, TcpListener, SslListener]
        }
    }.


get_config_or_die(ConfigFile) ->
    case erlcfg:new(ConfigFile) of
        {error, Reason} ->
            throw(Reason);
        Cfg ->
            Cfg
    end.

