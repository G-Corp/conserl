# File: Conserl.KV.ex
# This file was generated from conserl_kv.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Conserl.KV do
  def unquote(:"delete")(arg1) do
    :erlang.apply(:"conserl_kv", :"delete", [arg1])
  end
  def unquote(:"delete")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"delete", [arg1, arg2])
  end
  def unquote(:"delete")(arg1, arg2, arg3) do
    :erlang.apply(:"conserl_kv", :"delete", [arg1, arg2, arg3])
  end
  def unquote(:"get")(arg1) do
    :erlang.apply(:"conserl_kv", :"get", [arg1])
  end
  def unquote(:"get")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"get", [arg1, arg2])
  end
  def unquote(:"get_all")(arg1) do
    :erlang.apply(:"conserl_kv", :"get_all", [arg1])
  end
  def unquote(:"get_all")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"get_all", [arg1, arg2])
  end
  def unquote(:"keys")(arg1) do
    :erlang.apply(:"conserl_kv", :"keys", [arg1])
  end
  def unquote(:"keys")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"keys", [arg1, arg2])
  end
  def unquote(:"put")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"put", [arg1, arg2])
  end
  def unquote(:"put")(arg1, arg2, arg3) do
    :erlang.apply(:"conserl_kv", :"put", [arg1, arg2, arg3])
  end
  def unquote(:"put")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"conserl_kv", :"put", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"watch")(arg1) do
    :erlang.apply(:"conserl_kv", :"watch", [arg1])
  end
  def unquote(:"watch")(arg1, arg2) do
    :erlang.apply(:"conserl_kv", :"watch", [arg1, arg2])
  end
  def unquote(:"watch")(arg1, arg2, arg3) do
    :erlang.apply(:"conserl_kv", :"watch", [arg1, arg2, arg3])
  end
end
