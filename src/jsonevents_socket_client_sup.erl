-module(jsonevents_socket_client_sup).
-behaviour(supervisor).
-include("jsonevents.hrl").


-export([
        start_link/0,
        start_child/1
    ]).

-export([
        init/1
    ]).

start_link() ->
    Res = supervisor:start_link({local, ?SOCKET_CLIENT_SUP}, ?MODULE, []),
    error_logger:info_report({?MODULE, started}),
    Res.

start_child(ClientSocket) -> 
    supervisor:start_child(?SOCKET_CLIENT_SUP, [ClientSocket]).

init([]) ->
    {ok, 
        {
            {simple_one_for_one, 3, 10}, 
            [ 
                {
                    undefined, 
                    {jsonevents_socket_client_fsm, start_link, []}, 
                    transient, 
                    5000, 
                    worker, 
                    [jsonevents_socket_client_fsm]
                }
            ]
        }
    }.
