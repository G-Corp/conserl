

# Module conserl_catalog #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Consul Catalog API endpoints.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#datacenters-0">datacenters/0</a></td><td>
Return the list of datacenters.</td></tr><tr><td valign="top"><a href="#node-1">node/1</a></td><td>
Return node with given ID.</td></tr><tr><td valign="top"><a href="#node-2">node/2</a></td><td>
Return node with given ID and DC.</td></tr><tr><td valign="top"><a href="#nodes-0">nodes/0</a></td><td>
Return all nodes.</td></tr><tr><td valign="top"><a href="#nodes-1">nodes/1</a></td><td>
Return nodes for the given datacenter.</td></tr><tr><td valign="top"><a href="#service-1">service/1</a></td><td>
Return service with given Name.</td></tr><tr><td valign="top"><a href="#service-2">service/2</a></td><td>
Return service with given Name and DC.</td></tr><tr><td valign="top"><a href="#services-0">services/0</a></td><td>
Return all services.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>
Return services for the given datacenter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="datacenters-0"></a>

### datacenters/0 ###

<pre><code>
datacenters() -&gt; {ok, [binary()]} | {<a href="error.md#type-term">error:term()</a>}
</code></pre>
<br />

Return the list of datacenters

<a name="node-1"></a>

### node/1 ###

<pre><code>
node(ID::string() | binary()) -&gt; {ok, #{}} | {error, term()}
</code></pre>
<br />

Return node with given ID

<a name="node-2"></a>

### node/2 ###

<pre><code>
node(ID::string() | binary(), DC::string() | binary()) -&gt; {ok, #{}} | {error, term()}
</code></pre>
<br />

Return node with given ID and DC

<a name="nodes-0"></a>

### nodes/0 ###

<pre><code>
nodes() -&gt; {ok, [#{}]} | {error, term()}
</code></pre>
<br />

Return all nodes

<a name="nodes-1"></a>

### nodes/1 ###

<pre><code>
nodes(DC::string() | binary()) -&gt; {ok, [#{}]} | {error, term()}
</code></pre>
<br />

Return nodes for the given datacenter

<a name="service-1"></a>

### service/1 ###

<pre><code>
service(Name::string() | binary()) -&gt; {ok, #{}} | {error, term()}
</code></pre>
<br />

Return service with given Name

<a name="service-2"></a>

### service/2 ###

<pre><code>
service(Name::string() | binary(), DC::string() | binary()) -&gt; {ok, #{}} | {error, term()}
</code></pre>
<br />

Return service with given Name and DC

<a name="services-0"></a>

### services/0 ###

<pre><code>
services() -&gt; {ok, [#{}]} | {error, term()}
</code></pre>
<br />

Return all services

<a name="services-1"></a>

### services/1 ###

<pre><code>
services(DC::string() | binary()) -&gt; {ok, [#{}]} | {error, term()}
</code></pre>
<br />

Return services for the given datacenter

