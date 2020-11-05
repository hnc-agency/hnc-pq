PROJECT = hnc_pq
EDOC_OPTS = {stylesheet_file, "priv/edoc-style.css"},{todo,true}
EUNIT_OPTS = {verbose, true}

ERLANG_MK_BUILD_CONFIG = erlang-mk.build.config

include erlang.mk
