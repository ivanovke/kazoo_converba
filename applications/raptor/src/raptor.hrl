-ifndef(RAPTOR_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(APP, raptor).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-type http_method() :: kz_term:ne_binary(). %% HTTP Verbs in UPPERCASE
-type http_methods() :: kz_term:ne_binaries().
-type req_verb() :: http_method().

-define(RAPTOR_CONTEXT, #{}).

-define(RAPTOR_HRL, 'true').
-endif.
