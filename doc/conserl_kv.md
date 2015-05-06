

# Module conserl_kv #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Consul KV API endpoints.
__Authors:__ Gavin M. Roy ([`gavinmroy@gmail.com`](mailto:gavinmroy@gmail.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Return <code>Result</code> value for the given key.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Return <code>Result</code> for the given <code>Key</code> and specified query args (<code>QArgs</code>)
such as <code>{"dc", "production"}</code>.</td></tr><tr><td valign="top"><a href="#get_all-1">get_all/1</a></td><td>Return the values for all keys with the supplied <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#get_all-2">get_all/2</a></td><td>Return the values for all keys with the supplied <code>Prefix</code> passing in
aditional query arguments (<code>QArgs</code>), %% such as <code>{"dc", "production"}</code>.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>List all keys under the given <code>Prefix</code>.</td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td>List keys for the prefix.</td></tr><tr><td valign="top"><a href="#watch-1">watch/1</a></td><td>Blocking watch on the specified <code>Key</code>, returning <code>Result</code>.</td></tr><tr><td valign="top"><a href="#watch-2">watch/2</a></td><td>Asynchonous watch on the specified <code>Key</code>, calling the callback <code>Fun</code>
with the <code>Result</code>.</td></tr><tr><td valign="top"><a href="#watch-3">watch/3</a></td><td>Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments (<code>QArgs</code>), such as <code>{"dc", "production"}</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Return `Result` value for the given key.
<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Key, QArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Return `Result` for the given `Key` and specified query args (`QArgs`)
such as `{"dc", "production"}`.
<a name="get_all-1"></a>

### get_all/1 ###


<pre><code>
get_all(Prefix) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied `Prefix`.
<a name="get_all-2"></a>

### get_all/2 ###


<pre><code>
get_all(Prefix, QArgs) -&gt; list()
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

Return the values for all keys with the supplied `Prefix` passing in
aditional query arguments (`QArgs`), %% such as `{"dc", "production"}`.
<a name="keys-1"></a>

### keys/1 ###


<pre><code>
keys(Prefix) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List all keys under the given `Prefix`.
<a name="keys-2"></a>

### keys/2 ###


<pre><code>
keys(Prefix, QArgs) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Prefix = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {ok, list()} | {error, Reason}</code></li></ul>

List keys for the prefix. To add a separator for limiting the keys
returned, pass `{separator, Value}` in the `QArgs"" %% such
as ``{"dc", "production"}`.
<a name="watch-1"></a>

### watch/1 ###


<pre><code>
watch(Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Result = {ok, map()} | {error, Reason}</code></li></ul>

Blocking watch on the specified `Key`, returning `Result`.
<a name="watch-2"></a>

### watch/2 ###


<pre><code>
watch(Key, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified `Key`, calling the callback `Fun`
with the `Result`.
<a name="watch-3"></a>

### watch/3 ###


<pre><code>
watch(Key, QArgs, Fun) -&gt; {ok, <a href="#type-ref">ref()</a>}
</code></pre>

<ul class="definitions"><li><code>Key = list()</code></li><li><code>QArgs = list()</code></li><li><code>Result = {<a href="#type-ref">ref()</a>, map()} | {<a href="#type-ref">ref()</a>, {error, Reason}}</code></li></ul>

Asynchonous watch on the specified key, calling the callback 'Fun'
with the results of the call as Result, passing in aditional query
arguments (`QArgs`), such as `{"dc", "production"}`.
