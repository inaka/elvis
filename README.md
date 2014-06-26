![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis

Erlang Style Reviewer

## Usage

After adding **elvis** as a dependency and setting up its [configuration](#configutation), you can run it
form an erlang shell in the following two ways.

```erlang
elvis:rock().
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

This will try to load the configuration for **elvis** specified in the [configuration](http://www.erlang.org/doc/man/config.html).
For the **elvis** configuration information to be available, the application needs to be started. If no configuration is found
(i.e. an empty list `[]` is used) `invalid_config` will be thrown. To start the application enter the following command in the shell:

```erlang
application:start(elvis).
%= ok
```

```erlang
Config = [{src_dirs, ["src"]}, {rules, []}]
elvis:rock(Config).
```

This way of using **elvis** allows you to provide its configuration as an argument, `Config` which should have
a valid format. Since this is a project under development what a valid format is, is still undefined.

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
   },
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
