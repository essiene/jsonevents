-module(jsonevents_server_ssl).
-include("jsonevents.hrl").
-behaviour(gen_listener_tcp).

-export([
        start/0,
        start_link/1,
        upgrade_connection/2
    ]).

-export([
        init/1
    ]).

start() ->
    gen_listener_tcp:start({local, ?SSL_LISTENER}, ?MODULE, [erlcfg:new()], []).

start_link(Config) ->
    gen_listener_tcp:start_link({local, ?SSL_LISTENER}, ?MODULE, [Config], []).

init([Config]) ->
    {ok, 
        {
            Config:get(server.port.ssl, 8196), 
            [ 
                binary, 
                inet, 
                {active, false}, 
                {backlog, Config:get(server.backlog, 10)},
                {reuseaddr, true} 
            ], 
            
            { 
                ?MODULE,
                upgrade_connection,
                [Config]
            } 
        }
    }.


upgrade_connection(ClientSocket, Config) ->
    error_logger:info_msg("Upgrading connection ~p to SSL~n", [ClientSocket]),
    SslOpts = [
        {cacertfile, Config:get(server.ssl.cacerts, "/etc/jsonevents/cacerts.pem")},
        {certfile, Config:get(server.ssl.cert, "/etc/jsonevents/cert.pem")},
        {keyfile, Config:get(server.ssl.key, "/etc/jsonevents/key.pem")}
    ],

    case ssl:ssl_accept(ClientSocket, SslOpts) of
        {ok, SslSock} -> 
            jsonevents_session_sup:start_child(SslSock, ssl, ssl, ssl, ssl_closed, ssl_error);
        {error, Reason} ->
            error_logger:error_report([jsonevents, {action, upgrade_connection}, {error, Reason}])
    end.
