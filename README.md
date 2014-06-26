![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis

Erlang Style Reviewer

## Usage

After adding **elvis** as a dependency and setting up its [configuration](#configutation), you can run it
form an Erlang shell in the following two ways.

```erlang
elvis:rock().
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

This will try to load the configuration for **elvis** specified in the [application's configuration](http://www.erlang.org/doc/man/config.html),
for this to be available, the application needs to be started. If no configuration is found `invalid_config` will be thrown.

To start the application in the shell enter the following command:

```erlang
application:start(elvis).
%%= ok
```

Another option for using **elvis** from the shell is explicitly providing a configuration as an argument to ``rock()``:

```erlang
Config = [{src_dirs, ["src"]}, {rules, []}],
elvis:rock(Config).
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

`Config` should have a valid format, since this is a project under development the definition for *valid format* is still a
work in progress.

We have only presented results where all files were well-behaved (respect all the rules), so here's an example of how
it looks when files break some rules:

```
# ../../test/examples/fail_line_length.erl [FAIL]
  - line_length
    - Line 14 is too long: "    io:format(\"This line is 81 characters long and should be detected, yeah!!!\").".
    - Line 20 is too long: "    io:format(\"This line is 90 characters long and should be detected!!!!!!!!!!!!!!!!!!\").".
# ../../test/examples/fail_no_tabs.erl [FAIL]
  - no_tabs
    - Line 6 has a tab at column 0.
    - Line 15 has a tab at column 0.
# ../../test/examples/small.erl [OK]
```

## Configuration

To run **elvis** as described in the first option of the [Usage](#usage) section, you should include the following
environment values in your [configuration](http://www.erlang.org/doc/man/config.html) file:

```erlang
[
 {elvis,
  [
   {src_dirs, ["src", "test"]},
   {rules,
    [
     {elvis_style, line_length, [80]},
     {elvis_style, no_tabs, []},
     %% ..
    ]
   }
  ]
 }
]
```

The `src_dirs` entry is a list that indicates where **elvis** should look for the `*.erl` files that will be run through
each of the rules specified by the `rules` entry, which is list of rules with the following structure `{Module, Function, Args}`.

As you can see a rule is just a function, one that takes 3 arguments: **elvis**'s [configuration](#configuration), the path of the file and the
`Args` specified for the rule in the configuration. This means that you can define rules of your own as long as the functions
that implement them respect this arity.

There's currently no default configuration for **elvis**, but in the meantime you can take the one in `config/app.config`
as a starting point.

## References

Inspired on [HoundCI][houndci]

  [houndci]: https://houndci.com/
