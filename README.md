rebar3_ex_compiler
=====

A rebar3 generate to produce an OSv image from an OTP image

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_osv, ".*", {git, "https://github.com/....", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 osv compile
    ===> Fetching rebar3_osv
    ===> Compiling rebar3_osv
    <Plugin Output>

Or add it as a compile hook:

    {provider_hooks, [{pre, [{compile, {osv, compile}}]}]}.
