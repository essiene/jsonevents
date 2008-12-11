{application, jsonevents,
 [{description, "Simple events broadcaster"},
  {vsn, "0.1"},
  {modules, 
      [
          je_tcp_listener,
          jsonevents_tcp_listener
      ]
  },
  {registered, [jsonevents_tcp_listener]},
  {env, []},
  {applications, [kernel, stdlib]}
 ]
}.
