%% @hidden

-module(conserl_util).

%% API
-export([get/3,
         get/4,
         build_url/4,
         build_path/1,
         build_query/1,
         build_full_path/2]).

-include("conserl.hrl").

get(#state{host=Host, port=Port, acl=ACL}, Path, QArgs) when ACL =/= undefined ->
  http_get(Host, Port, Path, lists:merge(QArgs, [{acl, ACL}]), []);
get(State, Path, QArgs) ->
  http_get(State#state.host, State#state.port, Path, QArgs, []).

get(State, Path, QArgs, {timeout, Timeout}) ->
  http_get(State#state.host, State#state.port, Path, QArgs, [{timeout, Timeout}]);

get(State, Path, QArgs, Fun) ->
  http_get(State#state.host, State#state.port, Path, QArgs, {receiver, Fun}).

http_get(Host, Port, Path, QArgs, {receiver, Fun}) ->
  Receiver = spawn(fun() ->
    receive
      {http, Response} -> Fun(process_response(Response))
    end
  end),
  URL = build_url(Host, Port, Path, QArgs),
  lager:debug("http_get/5 with callback function: ~s", [URL]),
  httpc:request(get, {URL, []}, [], [{sync, false},
                {receiver, Receiver}]);

http_get(Host, Port, Path, QArgs, Options) ->
  URL = build_url(Host, Port, Path, QArgs),
  Response = httpc:request(get, {URL, []}, Options, []),
  case process_response(Response) of
    {ok, {error, Response}} -> {error, Response};
    Response -> Response
  end.

process_response({Ref, {{_Vsn, 200, _Reason}, Headers, Body}}) ->
  ContentType = proplists:get_value("content-type", Headers),
  case ContentType of
    "application/json" -> {Ref, json_decode(Body)};
    _ -> {Ref, Body}
  end;
process_response({Ref, {{_Vsn, _, Reason}, _Headers, _Body}}) -> {Ref, {error, Reason}};
process_response({error, Reason}) -> {error, Reason}.

json_decode(Value) when is_binary(Value) =:= true ->
  jsx:decode(Value);
json_decode(Value) when is_list(Value) =:= true ->
  jsx:decode(list_to_binary(Value));
json_decode(Value) ->
  lager:error("Unspported type ~p", [Value]),
  Value.

build_url(Host, Port, Path, QArgs) ->
  string:join(["http://", Host, ":", integer_to_list(Port), build_full_path(Path, QArgs)], "").

build_full_path(Path, []) ->
  string:join(["/", ?API_VERSION, build_path(Path)], "");

build_full_path(Path, QArgs) ->
  string:join(["/", ?API_VERSION, build_path(Path), "?", build_query(QArgs)], "").

build_path(Args) ->
  build_path(Args, []).

build_path([Part|Parts], Path) when is_atom(Part) =:= true ->
  build_path(Parts, string:join([Path, http_uri:encode(atom_to_list(Part))], "/"));

build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, http_uri:encode(Part)], "/"));

build_path([], Path) -> Path.

build_query(Args) ->
  build_query(Args, []).

build_query([{Key,Value}|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [string:join([http_uri:encode(atom_to_list(Key)),
                                                     encode_query_value(Value)], "=")]));

build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([http_uri:encode(Key),
                                                     encode_query_value(Value)], "=")]));

build_query([Key|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(atom_to_list(Key))]));

build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [http_uri:encode(Key)]));

build_query([], Parts) ->
  string:join(lists:sort(Parts), "&").

encode_query_value(Value) when is_atom(Value) =:= true ->
  http_uri:encode(atom_to_list(Value));

encode_query_value(Value) when is_list(Value) =:= true ->
  http_uri:encode(Value);

encode_query_value(Value) when is_integer(Value) =:= true ->
  integer_to_list(Value).
