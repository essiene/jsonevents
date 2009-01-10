{application, jsonevents,
 [{description, "Simple events broadcaster"},
  {vsn, "0.1"},
  {modules, 
      [
          gen_listener_tcp,
          jsonevents_server_tcp,
          jsonevents_server_ssl,
          jsonevents_session_sup,
          jsonevents_session,
          jsonevents_bus,
          jsonevents,
          jsonevents_test_server
      ]
  },
  {registered, 
      [
          jsonevents_server_tcp,
          jsonevents_server_ssl,
          jsonevents_session_sup,
          jsonevents_bus,
          jsonevents_sup
      ]
  },
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
