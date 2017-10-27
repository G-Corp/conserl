

# Erlang client for Consul #

Copyright (c) 2015 Gavin M. Roy

__Version:__ 0.1.0

__Authors:__ Gavin M. Roy ([`gavinmroy@gmail.com`](mailto:gavinmroy@gmail.com)), Carlo Cabanilla ([`carlo.cabanilla@gmail.com`](mailto:carlo.cabanilla@gmail.com)), Simon Ellefsen ([`https://github.com/simonellefsen`](mailto:https://github.com/simonellefsen)), mijkenator ([`https://github.com/mijkenator`](mailto:https://github.com/mijkenator)), Gregoire Lejeune ([`greg@g-corp.io`](mailto:greg@g-corp.io)).

[![Build Status](https://travis-ci.org/gmr/conserl.svg?branch=master)](https://travis-ci.org/gmr/conserl)

An Erlang client library for [Consul](http://consul.io).


## Requirements ##

* [Consul 0.5+](http://consul.io)

* Erlang 17.5+



## Environment Variables ##


<table width="100%" border="0" summary="environment variables">
<tr><td>Name</td><td>Type</td><td>Default</td><td>Description</td></tr>
<tr><td>hostname</td><td>list()</td><td><tt>127.0.0.1</tt></td><td>The IP address of the Consul server</td></tr>
<tr><td>port</td><td>pos_integer()</td><td><tt>8500</tt></td><td>The HTTP port of the Consul server</td></tr>
<tr><td>acl</td><td>list()</td><td>undefined</td><td>The ACL to use when making requests to the Consul server</td></tr>
</table>



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="conserl.md" class="module">conserl</a></td></tr>
<tr><td><a href="conserl_catalog.md" class="module">conserl_catalog</a></td></tr>
<tr><td><a href="conserl_kv.md" class="module">conserl_kv</a></td></tr></table>

