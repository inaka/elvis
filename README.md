![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis

Erlang Style Reviewer

## Usage

### Script

`elvis` can be turned into a script by executing `make escript`. This will
generate an `elvis` self-contained executable script, from which you can get
help by typing `elvis help`. A list of available commands can be shown using the
`--commands` option (i.e. `elvis --commands`).

To run `elvis` from the terminal use the `rock` command (i.e. `elvis
rock`). There's no need to specify a configuration file path if you have an
`elvis.config` file in the same location where you are executing the script,
otherwise a configuration file can be specified through the use of the
`--config` option.

```bash
elvis rock --config config/elvis.config
```

### Git hook

`elvis` can also be used as a [`git` pre-commit hook][pre-commit]
using the `git-hook` command, just use something like the following as
your pre-commit script:

```bash
#!/bin/sh
#
# Runs elvis to all Erlang staged files.

elvis git-hook
```

As the comment states, `elvis` will search for files with the `.erl` extension
among the staged files, get their staged content and run the rules specified in
the configuration. If any rule fails then `elvis` exits with a non-zero code,
which signals `git` that the commit shouldn't be made.

Make sure your pre-commit hook script is executable (i.e. by running
`chmod +x pre-commit`), otherwise `git` won't be able to run it.

### Erlang Shell

After adding `elvis` as a dependency to your project and setting up its
[configuration](#configuration), you can run it from an Erlang shell in the
following two ways.

```erlang
elvis:rock().
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

This will try to load the configuration for `elvis` specified in the
[application's configuration][config], for this to be available, the application
needs to be started. If no configuration is found `invalid_config` will be
thrown.

To start the application in the shell enter the following command:

```erlang
application:start(elvis).
%%= ok
```

Another option for using `elvis` from the shell is explicitly providing a
configuration as an argument to `rock/1`:

```erlang
Config = #{src_dirs => ["src"], rules => []},
elvis:rock(Config).
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

`Config` should have a valid format, since this is a project under development
the definition for *valid format* is still a work in progress.

We have only presented results where all files were well-behaved (respect all
the rules), so here's an example of how it looks when files break some og the
rules:

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

### Webhook

There's also a way to use `elvis` as a GitHub [webhook][webhooks] for
`pull request` (PR) events by calling the `webhook/1` function. This will add
a comment for each rule that is broken by a line in the files associated wiith
the PR.

Since GitHub's API needs a valid user and password to allow the creation of
comments, the parameters `github_user` and `github_password` need to be added to
`elvis`'s [configuration](#configuration).

The `webhook/1` function takes a map containing the keys `headers` and `body`,
whose values should be the map of headers and the body from the GitHub's event
request.

```erlang
Headers = #{<<"X-GitHub-Event">>, <<"pull_request">>},
Body = <<"{}">>, %% JSON data form GitHub's event.
Request = #{headers => Headers, body => Body},
elvis:webhook(Request).
```

## Configuration

To provide a default configuration for `elvis` you should either provide an
`elvis.config` file located in the root directory or set the following
environment values in your [configuration][config] file:

```erlang
[
 {
   elvis,
   [
    {config,
      #{src_dirs => ["src", "test"],
        rules    => [{elvis_style, line_length, [80]},
                     {elvis_style, no_tabs, []},
                     {elvis_style, macro_names, []},
                     {elvis_style, macro_module_names, []},
                     {elvis_style, operator_spaces, [{right, ","}, {right, "++"}, {left, "++"}]}
                    ]
       }
    },
    {github_user, "user"},
    {github_password, "password"}
   ]
 }
].
```

The `src_dirs` key is a list that indicates where `elvis` should look for the
`*.erl` files that will be run through each of the rules specified by the
`rules` entry, which is list of items with the following structure
`{Module, Function, Args}`.

As you can see a rule is just a function that takes 3 arguments: `elvis`'s
[configuration](#configuration), information of the file to be analyzed and
configuration `Args` (arguments) specified for the rule. This means that you can
define rules of your own as long as the functions that implement them respect
this arity.

There's currently no default configuration for `elvis`, but in the meantime
you can take the one in `config/app.config` as a starting point.

The GitHub configuration parameters `github_user` and `github_password` are
required only when `elvis` is used as a [webhook](#webhook).

## Dependencies

- Erlang/OTP 17.0
- make
- git

## References

Inspired on [HoundCI][houndci]

  [houndci]: https://houndci.com/
  [erlang]: http://www.erlang.org/download_release/24
  [make]: http://www.gnu.org/software/make/
  [git]: http://git-scm.com/
  [pre-commit]: http://git-scm.com/book/en/Customizing-Git-Git-Hooks#Client-Side-Hooks
  [config]: http://www.erlang.org/doc/man/config.html