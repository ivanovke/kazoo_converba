-ifndef(KZ_GRAPHQL_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(APP, kazoo_graphql).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(KZ_GRAPHQL_CONTEXT, #{}).

-define(KZ_GRAPHQL_HRL, 'true').
-endif.
