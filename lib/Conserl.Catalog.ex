# File: Conserl.Catalog.ex
# This file was generated from conserl_catalog.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Conserl.Catalog do
  def unquote(:"datacenters")() do
    :erlang.apply(:"conserl_catalog", :"datacenters", [])
  end
  def unquote(:"nodes")() do
    :erlang.apply(:"conserl_catalog", :"nodes", [])
  end
  def unquote(:"nodes")(arg1) do
    :erlang.apply(:"conserl_catalog", :"nodes", [arg1])
  end
  def unquote(:"node")(arg1, arg2) do
    :erlang.apply(:"conserl_catalog", :"node", [arg1, arg2])
  end
  def unquote(:"node")(arg1) do
    :erlang.apply(:"conserl_catalog", :"node", [arg1])
  end
  def unquote(:"services")() do
    :erlang.apply(:"conserl_catalog", :"services", [])
  end
  def unquote(:"services")(arg1) do
    :erlang.apply(:"conserl_catalog", :"services", [arg1])
  end
  def unquote(:"service")(arg1) do
    :erlang.apply(:"conserl_catalog", :"service", [arg1])
  end
  def unquote(:"service")(arg1, arg2) do
    :erlang.apply(:"conserl_catalog", :"service", [arg1, arg2])
  end
end
