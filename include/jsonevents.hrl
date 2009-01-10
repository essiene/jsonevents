-define(TCP_LISTENER, jsonevents_server_tcp).
-define(SSL_LISTENER, jsonevents_server_ssl).
-define(SUPERVISOR, jsonevents_sup).
-define(EVENT_BUS, jsonevents_bus).
-define(SESSION_SUP, jsonevents_session_sup).
-define(CLIENT_SESSION, jsonevents_session).


-record(session_state, {
        sock,
        inet,
        transport,
        tag_data,
        tag_closed,
        tag_error
    }).
