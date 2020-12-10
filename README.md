# elvis [![Build Status](https://github.com/inaka/elvis/workflows/build/badge.svg)](https://github.com/inaka/elvis)

![Elvis Presley dancing](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

Command-line interface for Elvis, the Erlang style reviewer.

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

## Installation

1. Clone the repo
2. `rebar3 compile`

## Usage

In any `elvis`-enabled product, `elvis rock` will trigger a rule check.

### Script

`elvis` can be turned into a script by executing `rebar3 escriptize`. This will
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

In `0.3.0` a new option was introduced in order to run elvis checks only on the source files that have changed since a particular branch of commit. Example usage would be `elvis git-branch origin/HEAD`.

## Benefits

- Enables consistency in style across all your code base.
- Encourages the development team to sit down and talk about code conventions.
- Allows continuous monitoring of code quality.
- Helps developers avoid repeated mistakes that can be automatically detected.
- Provides homogenisation among the different projects in a company, therefore facilitating project switching for developers and as well allowing easier code sharing between projects

### Webhook

There's also a way to use `elvis` as a GitHub [webhook][webhooks] for
`pull request` (PR) events by calling the `elvis_webhook:event/1` function. This will add
a comment in each file and rule that is broken, analyzing only the files
associated with the PR.

#### Running the webhook on your servers

Since GitHub's API needs a valid user and password to allow the creation of
reviews on PRs, the parameters `github_user` and `github_password` need to be
added to `elvis`'s [configuration](#configuration) and also the credentials used
must be from an admin of the repo or someone with permissions for requesting changes
on PRs.

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

If you only need to use `elvis` in the Erlang shell you might want to
consider only including the [`elvis_core`](https://github.com/inaka/elvis_core)
library as a dependency.

## Configuration

To provide a default configuration for `elvis` you should either create an
`elvis.config` file located in the root directory or set the following
environment values in your [configuration][config] file:

```erlang
[
 {
   elvis,
   [
    {config, [...]},
    {output_format, plain},

    %% Only necessary for the 'webhook' functionality
    {github_user, "user"},
    {github_password, "password"}
   ]
 }
].
```

The `config` and `output_format` are explained in [`elvis_core`](https://github.com/inaka/elvis_core).

The GitHub configuration parameters `github_user` and `github_password` are
required only when `elvis` is used as a [webhook](#webhook).

### elvis.config

In your `elvis.config` file you can setup which rules should be
applied, on what files and in which directories to do it.

The configuration is in Erlang format, it is not that hard to write
but it is easier if you use the `elvis.config` file in this reposiotry
as a template.

In the `elvis.config` file you create an elvis config where for a set
of directories, you want to run a ruleset (or specific rules) on a set
of files.

For example, configure to check all erlang files under the `src`
directory using the ruleset `erl_files`:

```erlang
[
 {
   elvis,
   [
    {config,
     [#{dirs => ["src"],
        filter => "*.erl",
        ruleset => erl_files
       }
     ]
    }
   ]
 }
].
```

You can use four different rulesets `erl_files`, `makefiles`, `rebar_config` or `elvis_config`.

## Implemented Rules

A reference of all rules implemented in Elvis can be found in this wiki page:
[Rules](https://github.com/inaka/elvis_core/wiki/Rules).

## User Defined Rules

If you have implemented an Elvis rule that's in your local repo or in one of
your dependencies, you can add this rule to your `elvis.config` file and
tell Elvis where to find the `.beam` that contains the compiled rule using
the `--code-path` (`-p`) option.

For example if the rule is in one of your deps, you could run Elvis in the
following way:

```shell
elvis rock -p deps/elvis_rules/ebin -c elvis.config
```

## Dependencies

- Erlang/OTP 18+
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
