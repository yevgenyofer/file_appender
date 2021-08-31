file_appender
=====

An OTP application

Build
-----

    $ rebar3 compile

Build & Run
-----

    $ rebar3 shell

Usage
-----

    $ file_appender:append("filename.txt", "string").
    $ file_appender:is_file_opened().

Tests
-----

    $ rebar3 eunit
# file_appender
