PROJECT = hnc_pq
PROJECT_DESCRIPTION = Erlang Priority Queues
PROJECT_VERSION = 0.1.2
EDOC_OPTS = {stylesheet_file, "priv/edoc-style.css"},{todo,true},{def, [{vsn, "${PROJECT_VERSION}"}]}
EUNIT_OPTS = {verbose, true}

ERLANG_MK_BUILD_CONFIG = erlang-mk.build.config

include erlang.mk
