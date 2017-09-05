ROOT = ../..
PROJECT = kazoo_documents

TEST_EBINS = $(ROOT)/applications/crossbar/ebin

all: compile

eunit: kz-test

test: kz-test

kz-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config/

include $(ROOT)/make/kz.mk

compile-test: compile-test-deps

compile-test-deps:
	$(MAKE) -C $(ROOT)/core/kazoo_stdlib compile-test
	$(MAKE) -C $(ROOT)/core/kazoo_config compile-test
