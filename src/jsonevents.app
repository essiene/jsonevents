{application, jsonevents,
 [{description, "Simple events broadcaster"},
  {vsn, "0.1"},
  {modules, 
      [
          gen_listener_tcp,
          jsonevents_server_tcp,
          jsonevents_socket_client_sup,
          jsonevents_socket_client_fsm
      ]
  },
  {registered, 
      [
          jsonevents_server_tcp,
          jsonevents_socket_client_sup 
      ]
  },
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
