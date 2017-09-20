ROOT = ../..
PROJECT = kazoo_couch

all: compile

compile: compile-first

compile-first:
	$(MAKE) -C $(ROOT)/core/kazoo_data/ $(ACTION)

include $(ROOT)/make/kz.mk
