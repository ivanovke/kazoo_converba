ROOT = ../..
PROJECT = kazoo_data

SOURCES = src/kz_data.erl src/*.erl

ERLC_OPTS += +'{parse_transform, kz_perf_transform}' +'{lager_extra_sinks, [data]}'

all: first compile

-include $(ROOT)/make/kz.mk

first: ebin/kz_perf_transform.beam

ebin/kz_perf_transform.beam: src/kz_perf_transform.erl
	erlc -v +debug_info $(PA) -o ebin/ $?
