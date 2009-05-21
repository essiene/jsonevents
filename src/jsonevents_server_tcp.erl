-module(jsonevents_server_tcp).
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

start() ->
    gen_listener_tcp:start({local, ?TCP_LISTENER}, ?MODULE, [erlcfg:new()], []).

start_link(Config) ->
    gen_listener_tcp:start_link({local, ?TCP_LISTENER}, ?MODULE, [Config], []).

init([Config]) ->
    case Config:get(server.port.tcp) of
        {error, {not_found, server.port.tcp}} ->
            error_logger:warning_report("TCP port unspecified. The TCP listener will not start"),
            ignore;
        Port ->
            {
                ok, 
                {Port, [binary, inet, 
                        {active, false}, 
                        {backlog, Config:get(server.backlog, 10)},
                        {reuseaddr, true}]
                },
                nil
            }
    end.

handle_accept(Sock, State) -> 
    jsonevents_session_sup:start_child(Sock),
    {noreply, State}.

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
