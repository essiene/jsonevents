EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLC=erlc
ERLC_FLAGS= -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)
ERL=erl -noshell -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

$(EBIN_DIR)/%.beam: %.erl
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(EBIN_DIR)/%.app: %.app
	@mkdir -p $(EBIN_DIR)
	@cp $< $@


OBJECTS=je_tcp_listener \
		jsonevents_tcp_listener

APP=jsonevents


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam} ${APP:%=$(EBIN_DIR)/%.app}
	@echo All Done

run: all
	$(ERL) -s jsonevents_tcp_listener


clean:
	@rm -f *_lexer.erl
	@rm -f *_parser.erl
	@rm -rf $(EBIN_DIR)
	@rm -f erl_crash.dump
