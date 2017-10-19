

# Module conserl_catalog #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Consul Catalog API endpoints.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_service_nodes-1">get_service_nodes/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_service_nodes-2">get_service_nodes/2</a></td><td></td></tr><tr><td valign="top"><a href="#node-1">node/1</a></td><td></td></tr><tr><td valign="top"><a href="#node-2">node/2</a></td><td></td></tr><tr><td valign="top"><a href="#nodes-0">nodes/0</a></td><td>Return nodes as a list.</td></tr><tr><td valign="top"><a href="#nodes-1">nodes/1</a></td><td>Return nodes as a list.</td></tr><tr><td valign="top"><a href="#nodes_with_tags-0">nodes_with_tags/0</a></td><td></td></tr><tr><td valign="top"><a href="#nodes_with_tags-1">nodes_with_tags/1</a></td><td></td></tr><tr><td valign="top"><a href="#services-0">services/0</a></td><td>Return services as a list.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Return services as a proplist of service name a list of nodes.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_service_nodes-1"></a>

### get_service_nodes/1 ###

<pre><code>
get_service_nodes(ServiceName::string()) -&gt; {ok, [{string(), string(), [{string(), [string()]}]}]}
</code></pre>
<br />

<a name="get_service_nodes-2"></a>

### get_service_nodes/2 ###

<pre><code>
get_service_nodes(ServiceName::string(), Tag::string()) -&gt; {ok, [{string(), string(), [{string(), [string()]}]}]}
</code></pre>
<br />

<a name="node-1"></a>

### node/1 ###

<pre><code>
node(Node::string()) -&gt; [{binary(), list()}]
</code></pre>
<br />

<a name="node-2"></a>

### node/2 ###

`node(Node, DC) -> any()`

<a name="nodes-0"></a>

### nodes/0 ###

<pre><code>
nodes() -&gt; list()
</code></pre>
<br />

Return nodes as a list

<a name="nodes-1"></a>

### nodes/1 ###

<pre><code>
nodes(DC) -&gt; list()
</code></pre>
<br />

Return nodes as a list

<a name="nodes_with_tags-0"></a>

### nodes_with_tags/0 ###

<pre><code>
nodes_with_tags() -&gt; {ok, [{string(), string(), [{string(), [string()]}]}]}
</code></pre>
<br />

<a name="nodes_with_tags-1"></a>

### nodes_with_tags/1 ###

`nodes_with_tags(DC) -> any()`

<a name="services-0"></a>

### services/0 ###

<pre><code>
services() -&gt; list()
</code></pre>
<br />

Return services as a list

<a name="services-1"></a>

### services/1 ###

<pre><code>
services(DC) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>DC = list()</code></li></ul>

Return services as a proplist of service name a list of nodes

