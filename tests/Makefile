EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERLC_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR) -W -o $(EBIN_DIR)
ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR)

$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) $<


OBJECTS=


test: compile
	erl $(ERL_FLAGS) -noshell $(foreach t, $(OBJECTS), -s $(t) test) -s init stop
	@echo finished running tests


compile: ${OBJECTS:%=$(EBIN_DIR)/%.beam}
	@echo all done compiling

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
