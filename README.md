watermark
=========

[![Build Status](https://img.shields.io/travis/altenwald/watermark/master.svg)](https://travis-ci.org/altenwald/watermark)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/watermark.svg)](https://codecov.io/gh/altenwald/watermark)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/watermark.svg)](https://raw.githubusercontent.com/altenwald/watermark/master/COPYING)


Watermark generic implementation to let you create your own watermark system based on a high limit to activate a state and a low level to deactivate that state and back to "normal". It's intended you can reach an overload level as well where it's not possible to continue incrementing the values.

The way to increase/decrease the levels is based on a counter implemented inside of the generic behaviour.

Getting started
---------------

You'll need to add the dependency first...

```erlang
{deps, [
    {watermark, {git, "git@github.com:altenwald/watermark", {branch, master}}}
]}.
```

The recommended Erlang/OTP version is 20.2 but it's intended to work from 19.0.

You can add this to `rebar.config` to use the templates once the dependency is installed this way:

```
$ rebar3 new watermark name=simple_watermark
```

The template could be found in the `priv/templates` directory of this project to further information.

The way to proceed once you have your code wrote is using:

```erlang
{ok, PID} = gen_watermark:start_link(simple_watermark, [], [], 0, 25, 75, 100).
```

That function is in charge to create a process using the implementation of the module `simple_watermark`. The initial `Args` for the process is an empty list and the options for the server are empty as well in our example.

The values following are intended to be: initial value, low level watermark, high level watermark and overload (or overflow).

Then you can use the functions:

```erlang
gen_watermark:incr(PID),
gen_watermark:incr(PID, 10),
gen_watermark:decr(PID),
gen_watermark:decr(PID, 10),
```

To increase and decrease the internal value. You can also use this function to obtain the internal information:

```erlang
{normal, 0} = gen_watermark:get_state(PID),
```

Troubleshooting
---------------

If you need help with the library, you find a bug or want to collaborate to fix or improve, don't hesitate to create a pull request or an issue to let us know about it.

Enjoy!
