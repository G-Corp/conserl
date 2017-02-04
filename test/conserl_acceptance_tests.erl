-module(conserl_acceptance_tests).

-include("conserl.hrl").

-include_lib("eunit/include/eunit.hrl").

simple_kv_put_get_test() ->
  application:ensure_all_started(conserl),
  Key = binary_to_list(<<"foo">>),
  Value = binary_to_list(<<"bar">>),
  conserl_kv:put(Key, Value),
  {ok, Response} = conserl_kv:get(Key),
  ?assertEqual(Value, maps:get(value, Response)).

simple_kv_get_not_found_test() ->
  Key = binary_to_list(<<"baz">>),
  ?assertEqual({error, "Not Found"}, conserl_kv:get(Key)).



