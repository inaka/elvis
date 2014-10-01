![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis

Erlang Style Reviewer

## Contact Us
For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](https://www.hipchat.com/gpBpW3SsT).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/elvis/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

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
`--config` (or just `-c`) option.

```bash
elvis rock --config config/elvis.config
```

### Webhook

There's also a way to use `elvis` as a GitHub [webhook][webhooks] for
`pull request` (PR) events by calling the `webhook/1` function. This will add
a comment in each file and rule that is broken, analyzing only the files
associated with the PR.

#### [elvis.inakalabs.com][elvis-web]

This Website is available for you to add `elvis` as a webhook in your GitHub's
repositories. Just log in with your GitHub credentials and the site will pull
all the repos for which you have permissions to add webhooks. The `elvis`
webhook can always be deactivated at any time using the same mechanism.

When activating the webhook, the site will use the GitHub API to add the user
`elvisinaka` as a collaborator to your repo, so that it can create comments
on its pull requests. If the repo belongs to an organization, a **Services**
team is created instead (if it doesn't exist already), then this team is added
to the repo and the `elvisinaka` user is added to that team.

#### Running the webhook on your servers

Since GitHub's API needs a valid user and password to allow the creation of
comments on PRs, the parameters `github_user` and `github_password` need to be
added to `elvis`'s [configuration](#configuration).

The `webhook/1` function takes a map containing the keys `headers` and `body`,
whose values should be the map of headers and the body from the GitHub's event
request.

```erlang
Headers = #{<<"X-GitHub-Event">>, <<"pull_request">>},
Body = <<"{}">>, %% JSON data form GitHub's event.
Request = #{headers => Headers, body => Body},
elvis:webhook(Request).
```

### Git hook

`elvis` can also be used as a [`git` pre-commit hook][pre-commit]
using the `git-hook` command, just use something like the following as
your pre-commit script:

```bash
#!/bin/sh
#
# Runs elvis rules to staged files where applicable.

elvis git-hook
```

As the comment states, `elvis` will search for files that match the `filter` of
each rule group (see [configuration](#configuration)) among the staged files,
get their staged content and run the rules specified in the configuration.
If any rule fails then `elvis` exits with a non-zero code,
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
elvis [application configuration][config], for this to be available, the
application needs to be started. If no configuration is found `invalid_config`
will be thrown.

To start the application in the shell enter the following command:

```erlang
application:start(elvis).
%%= ok
```

Another option for using `elvis` from the shell is explicitly providing a
configuration as an argument to `rock/1`:

```erlang
Config = [#{dirs => ["src"], filter => "*.erl", rules => []}],
elvis:rock(Config).
%%+ # src/elvis.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

**IMPORTANT**: `Config` should have a valid format, but since this is a project
under development the definition for *valid format* is still a work in progress.
If the configuration format changes though, the example configuration files and
the documentation in this README will be updated.

We have only presented results where all files were well-behaved (respect all
the rules), so here's an example of how it looks when files break some of the
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
     [#{dirs => ["src", "test"],
        filter => "*.erl",
        rules    => [{elvis_style, line_length, [80]},
                     {elvis_style, no_tabs, []},
                     {elvis_style, macro_names, []},
                     {elvis_style, macro_module_names, []},
                     {elvis_style, operator_spaces, [{right, ","},
                                                     {right, "++"},
                                                     {left, "++"}
                                                    ]}
                    ]
       },
      #{dirs => ["."],
        filter => "Makefile",
        rules => [{elvis_project, no_deps_master_erlang_mk, []}]
       },
      #{dirs => ["."],
        filter => "rebar.config",
        rules => [{elvis_project, no_deps_master_rebar, []}]
       }
     ]
    },
    %% Only necessary for the 'webhook' functionality
    {github_user, "user"},
    {github_password, "password"}
   ]
 }
].
```

The `dirs` key is a list that indicates where `elvis` should look for the
files that match `filter`, which will be run through each of the rules
specified by the `rules` entry, which is list of items with the following
structure `{Module, Function, Args}`.

As you can see a rule is just a function and it takes 3 arguments: `elvis`'s
`config` entry from its [configuration](#configuration); the file to be
analyzed; and some configuration `Args` (arguments) specified for the rule.
This means you can define rules of your own as long as the functions that
implement them respect this arity.

There's currently no default configuration for `elvis`, but in the meantime
you can take the one in `config/elvis.config` as a starting point.

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
  [webhooks]: https://developer.github.com/v3/repos/hooks/
  [elvis-web]: http://elvis.inakalabs.com/
