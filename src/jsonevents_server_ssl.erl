-module(jsonevents_server_ssl).
-include("jsonevents.hrl").
-behaviour(gen_listener_tcp).

-export([
        start/0,
        start_link/1
    ]).

-export([
        init/1,
        handle_accept/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-record(server_state, {
        cacertfile,
        certfile,
        keyfile}).


start() ->
    gen_listener_tcp:start({local, ?SSL_LISTENER}, ?MODULE, [erlcfg:new()], []).

start_link(Config) ->
    gen_listener_tcp:start_link({local, ?SSL_LISTENER}, ?MODULE, [Config], []).

init([Config]) ->
    case Config:get(server.port.ssl) of
        {error, {not_found, server.port.ssl}} ->
            error_logger:warning_report("SSL port unspecified. The SSL listener will not start"),
            ignore;
        Port ->
            {ok, 
                {Port, [binary, inet, 
                        {active, false}, 
                        {backlog, Config:get(server.backlog, 10)},
                        {reuseaddr, true}]}, 
                #server_state{cacertfile = Config:get(server.ssl.cacerts, "/etc/jsonevents/cacerts.pem"), 
                              certfile = Config:get(server.ssl.cert, "/etc/jsonevents/cert.pem"), 
                              keyfile = Config:get(server.ssl.key, "/etc/jsonevents/key.pem")}
            } 
    end.

handle_accept(Sock, St) ->
    error_logger:info_msg("Upgrading connection ~p to SSL~n", [Sock]),
    SslOpts = [{cacertfile, St#server_state.cacertfile},
        {certfile, St#server_state.certfile},
        {keyfile, St#server_state.keyfile}
    ],

    case ssl:ssl_accept(Sock, SslOpts) of
        {ok, SslSock} -> 
            jsonevents_session_sup:start_child(SslSock, ssl, ssl, ssl, ssl_closed, ssl_error),
            {noreply, St};
        {error, Reason} ->
            error_logger:error_report([jsonevents, {action, upgrade_connection}, {error, Reason}]),
            {noreply, St}
    end.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
