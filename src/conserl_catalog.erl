% @doc Consul Catalog API endpoints

-module(conserl_catalog).

-export([
         datacenters/0
         , nodes/0
         , nodes/1
         , node/1
         , node/2
         , services/0
         , services/1
         , service/1
         , service/2
        ]).

% @doc
% Return the list of datacenters
% @end
-spec datacenters() -> {ok, [binary()]} | {error: term()}.
datacenters() ->
  gen_server:call(conserl, {get, [catalog, datacenters], []}).

% @doc
% Return all nodes
% @end
-spec nodes() -> {ok, [map()]} | {error, term()}.
nodes() ->
  get_nodes([]).

% @doc
% Return nodes for the given datacenter
% @end
-spec nodes(DC :: string() | binary()) -> {ok, [map()]} | {error, term()}.
nodes(DC) when is_list(DC) ->
  get_nodes([{dc, DC}]);
nodes(DC) when is_binary(DC) ->
  ?MODULE:nodes(bucs:to_string(DC)).

get_nodes(Args) ->
  gen_server:call(conserl, {get, [catalog, nodes], Args}).

% @doc
% Return node with given ID and DC
% @end
-spec node(ID :: string() | binary(), DC :: string() | binary()) -> {ok, map()} | {error, term()}.
node(ID, DC) when is_binary(ID) ->
  ?MODULE:node(bucs:to_string(ID), DC);
node(ID, DC) when is_binary(DC) ->
  ?MODULE:node(ID, bucs:to_string(DC));
node(ID, DC) when is_list(ID), is_list(DC) ->
  get_node(ID, [{dc, DC}]).

% @doc
% Return node with given ID
% @end
-spec node(ID :: string() | binary()) -> {ok, map()} | {error, term()}.
node(ID) when is_binary(ID) ->
  ?MODULE:node(bucs:to_string(ID));
node(ID) when is_list(ID) ->
  get_node(ID, []).

get_node(ID, Args) ->
  gen_server:call(conserl, {get, [catalog, node, ID], Args}).

% @doc
% Return all services
% @end
-spec services() -> {ok, [map()]} | {error, term()}.
services() ->
  get_services([]).

% @doc
% Return services for the given datacenter
% @end
-spec services(DC :: string() | binary()) -> {ok, [map()]} | {error, term()}.
services(DC) when is_list(DC) ->
  get_services([{dc, DC}]);
services(DC) when is_binary(DC) ->
  services(bucs:to_string(DC)).

get_services(Args) ->
  gen_server:call(conserl, {get, [catalog, services], Args}).

% @doc
% Return service with given Name
% @end
-spec service(Name :: string() | binary()) -> {ok, map()} | {error, term()}.
service(Name) when is_binary(Name) ->
  service(bucs:to_string(Name));
service(Name) when is_list(Name) ->
  get_service(Name, []).

% @doc
% Return service with given Name and DC
% @end
-spec service(Name :: string() | binary(), DC :: string() | binary()) -> {ok, map()} | {error, term()}.
service(Name, DC) when is_binary(Name) ->
  service(bucs:to_string(Name), DC);
service(Name, DC) when is_binary(DC) ->
  service(Name, bucs:to_string(DC));
service(Name, DC) when is_list(Name), is_list(DC) ->
  get_service(Name, [{dc, DC}]).

get_service(Name, Args) ->
  gen_server:call(conserl, {get, [catalog, service, Name], Args}).

