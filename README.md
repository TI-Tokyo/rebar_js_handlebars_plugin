rebar_js_concatenator_plugin
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar_js_concatenator_plugin, {git, "https://host/user/rebar_js_concatenator_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_js_concatenator_plugin
    ===> Fetching rebar_js_concatenator_plugin
    ===> Compiling rebar_js_concatenator_plugin
    <Plugin Output>
