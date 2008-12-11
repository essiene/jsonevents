NAME=jsonevents
VERSION=0.1
ERL_LIB=/usr/lib/erlang/lib
EBIN_DIR=ebin
INCLUDE_DIR=include
SRC_DIR=src

all: 
	@cd src;make
	@echo All Done

run: all
	@cd src;make run

test: all
	@cd tests;make
	@echo Tests Done

clean:
	@cd src;make clean
	@cd tests;make clean
	@rm -rf ebin
	@rm -f erl_crash.dump
	@rm -f *.tar.gz
	@rm -rf $(NAME)-$(VERSION)

install: all
	@mkdir -p $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/{ebin,include,src}
	@cp $(EBIN_DIR)/* $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/ebin/
	@cp $(INCLUDE_DIR)/* $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/include/
	@cp $(SRC_DIR)/* $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/src/

dist: clean
	@mkdir -p $(NAME)-$(VERSION)
	@cp TODO README Makefile $(NAME)-$(VERSION)
	@cp -r include src tests $(NAME)-$(VERSION)
	@tar -czvf $(NAME)-$(VERSION).tar.gz $(NAME)-$(VERSION)
	@rm -rf $(NAME)-$(VERSION)

rpm: dist
	@rm -rf /usr/src/redhat/SOURCES/$(NAME)*
	@rm -rf /usr/src/redhat/RPMS/i386/$(NAME)*
	@mv $(NAME)-$(VERSION).tar.gz /usr/src/redhat/SOURCES/
	@cp $(NAME).spec /usr/src/redhat/SPECS/
	@rpmbuild -bb /usr/src/redhat/SPECS/$(NAME).spec
	@mv /usr/src/redhat/RPMS/i386/$(NAME)*.rpm .
