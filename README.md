elogstash
=====

[![Build Status](https://img.shields.io/travis/manuel-rubio/elogstash/master.svg)](https://travis-ci.org/manuel-rubio/elogstash)
[![Codecov](https://img.shields.io/codecov/c/github/manuel-rubio/elogstash.svg)](https://codecov.io/gh/manuel-rubio/elogstash)
[![License: LGPL 2.1](https://img.shields.io/github/license/manuel-rubio/elogstash.svg)](https://raw.githubusercontent.com/manuel-rubio/elogstash/master/LICENSE)

Logstash library to send information using UDP or TCP avoiding bottlenecks

Build
-----

    $ rebar3 compile

To Play
-------

    $ rebar3 shell

To Include as Dependency
------------------------

In your `rebar.config` (rebar3):

```erlang
{deps, [
    {elogstash, {git, "https://github.com/manuel-rubio/elogstash", {branch, master}}
]}
```

In your `rebar.config` (rebar2):

```erlang
{deps, [
    {elogstash, ".*", {git, "https://github.com/manuel-rubio/elogstash", {branch, master}}
]}
```

In your `Makefile` (erlang.mk):

```makefile
DEPS = elogstash
dep_elogstash = git https://github.com/manuel-rubio/elogstash.git master
```

Configuration
-------------

The configuration file only has a couple of parameters. They could be added in your code to implement the configuration from other way as well.

Using the configuration file (sys.config or app.config):

```erlang
{elogstash, [
    {connection, {tcp, {"localhost", 5000}}},
    {max_worker, 10}
]},
```

The connection could be done using a string for the name (like in the example: `"localhost"`) or using an IP address in the way Erlang implement them (i.e. `{127,0,0,1}`).

The transports available at this moment are `tcp`, `udp` or `file`.

In case of `file` you have to configure the basename of the file instead of the host and the kind of rotation you want: `hourly`, `daily` or `monthly`. The file could contains a path and it could be absolute or relative path but I recommend to use absolute path always.

The `max_workers` is the max number of workers the pool will create. The minimum is the half so, if you configure 10 workers, the start number of connections will be 5. The `file` backend force to use only 1 max worker.

The configuration could be done in the code configuring `elogstash` as a dependency but only to load and then:

```erlang
application:load(elogstash),
application:set_env(elogstash, connection, {{127,0,0,1}, 5000}),
application:set_env(elogstash, max_worker, 10),
elogstash:start(),
```

Enjoy!
