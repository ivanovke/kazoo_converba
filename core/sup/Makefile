ROOT = ../..
PROJECT = sup

ESCRIPT = sup
CLEAN_MOAR = clean-escript

all: $(ESCRIPT)

$(ESCRIPT):
	@$(REBAR) compile escriptize

clean-escript:
	$(if $(wildcard sup), @$(REBAR) clean)

include $(ROOT)/make/kz.mk
