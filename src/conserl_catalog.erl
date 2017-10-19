%%% @doc Consul Catalog API endpoints

-module(conserl_catalog).

-export([nodes/0, nodes/1, services/0, services/1, node/1, node/2,
         nodes_with_tags/0, nodes_with_tags/1,
         get_service_nodes/2, get_service_nodes/1]).

% get all node information as is from consul
-spec node(string()) -> [{binary(), list()}].
node(Node) -> node(Node, none).
node(Node, DC) ->
  Args = case DC of
           none -> [];
           _ -> [{dc, DC}]
         end,
  case gen_server:call(conserl, {get, [catalog, node, Node], Args}) of
    {ok, Result} -> Result;
    Other -> Other
  end.

% return list of nodes with {service, tag} informations
% [{NodeName, IP, [{ServiceNamw, [Tag1, Tag2, ....]}]}, ..... ]
-spec nodes_with_tags() -> {ok, [{string(), string(), [{string(), [string()]}]}]}.
nodes_with_tags() -> nodes_with_tags(none).
nodes_with_tags(DC) ->
  {ok, Nodes} = conserl_catalog:nodes(DC),
  {ok, [ {Name, IP, get_node_tags(DC, Name)} || {Name, IP} <- Nodes ]}.

% return same struct as nodes_with_tags but filtered by ServiceName and Tag
% get_service_nodes("postgres") -> get all postgres nodes
% get_service_nodes("postgres", "slave") -> get all slave postgres nodes
-spec get_service_nodes(string()) -> {ok, [ {string(), string(), [{string(), [string()]}] }]}.
get_service_nodes(ServiceName)          -> get_service_nodes(ServiceName, any, none).
-spec get_service_nodes(string(), string()) -> {ok, [{string(), string(), [{string(), [string()]}]}]}.
get_service_nodes(ServiceName, Tag)     -> get_service_nodes(ServiceName, Tag, none).
get_service_nodes(ServiceName, Tag, DC) ->
  {ok, Nodes} = conserl_catalog:nodes_with_tags(DC),
  Fun = case Tag of
          any -> fun({_, _, Tags}) -> [ TL  || {SN, TL} <- Tags, SN =:= ServiceName] =/= [] end;
          _  -> fun({_, _, Tags}) ->
                    lists:flatten([ [ TLE || TLE <- TL, TLE =:= Tag]
                                    || {SN, TL} <- Tags, SN =:= ServiceName ]) =/= []
                end
        end,
  lists:filter(Fun, Nodes).

get_node_tags(DC, Name) ->
  lists:flatten([ extract_tags(E) || E <- node(Name, DC)]).

extract_tags({<<"Services">>, S}) ->
  Fun = fun(X) when is_binary(X) -> binary_to_list(X);
           (X) -> X end,
  [ {binary_to_list(ServiceName), [ lists:flatten(lists:map(Fun, PropValue))
                                    || {PropName, PropValue} <- L, PropName =:= <<"Tags">>]}
    || {ServiceName, L} <- S ];
extract_tags(_) -> [].


%% @spec nodes() -> list()
%% @doc Return nodes as a list
%% @end
%%
nodes() ->
  conserl_catalog:nodes(none).

%% @spec nodes(DC) -> list()
%% where

%%       DC = list()
%% @doc Return nodes as a list
%% @end
%%
nodes(DC) ->
  Args = case DC of
           none -> [];
           _ -> [{dc, DC}]
         end,
  case gen_server:call(conserl, {get, [catalog, nodes], Args}) of
    {ok, Result} -> {ok, build_node_list(Result)};
    Other -> Other
  end.

%% @spec services() -> list()
%% @doc Return services as a list
%% @end
%%
services() ->
  services(none).

%% @spec services(DC) -> list()
%% where
%%       DC = list()
%% @doc Return services as a proplist of service name a list of nodes
%% @end
%%
services(DC) ->
  Args = case DC of
           none -> [];
           _ -> [{dc, DC}]
         end,
  case gen_server:call(conserl, {get, [catalog, services], Args}) of
    {ok, Result} -> {ok, build_service_list(Result)};
    Other -> Other
  end.


build_node_list(Nodes) -> build_node_list(Nodes, []).
build_node_list([], Acc) -> Acc;
build_node_list([H|T], Acc) ->
  build_node_list(T, lists:append(Acc,
                                  [{binary_to_list(proplists:get_value(<<"Node">>, H)),
                                    binary_to_list(proplists:get_value(<<"Address">>, H))}])).

build_service_list(Nodes) -> build_service_list(Nodes, []).
build_service_list([], Acc) -> Acc;
build_service_list([{K, V}|T], Acc) ->
  build_service_list(T,
                     lists:append(Acc,
                                  [{binary_to_list(K), [binary_to_list(N) || N <- V]}])).
