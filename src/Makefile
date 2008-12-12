EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLC=erlc
ERLC_FLAGS= -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)
ERL=erl -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

$(EBIN_DIR)/%.beam: %.erl
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(EBIN_DIR)/%.app: %.app
	@mkdir -p $(EBIN_DIR)
	@cp $< $@


OBJECTS=gen_listener_tcp \
		jsonevents_server_tcp \
		jsonevents_socket_client_sup  \
		jsonevents_socket_client_fsm \
		jsonevents_bus \
		jsonevents_sup \
		jsonevents \
		jsonevents_test_server

APP=jsonevents


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam} ${APP:%=$(EBIN_DIR)/%.app}
	@echo All Done

run: all
	@erl -noinput -pa $(EBIN_DIR) -s jsonevents_test_server

clean:
	@rm -f *_lexer.erl
	@rm -f *_parser.erl
	@rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump
