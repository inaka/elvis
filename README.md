[![Stories in Ready](https://badge.waffle.io/inaka/elvis.png?label=ready&title=Ready)](https://waffle.io/inaka/elvis)

![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis [![Build Status](https://travis-ci.org/inaka/elvis.svg?branch=master)](https://travis-ci.org/inaka/elvis)

Command-line interface for Elvis, the Erlang style reviewer.

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

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

When activating the webhook, the site will use the [GitHub API](https://developer.github.com/v3/) to add the user
`elvisinaka` as a collaborator to your repo, so that it can create comments
on its pull requests. If the repo belongs to an organization, a **Services**
team is created instead (if it doesn't exist already), then this team is added
to the repo and the `elvisinaka` user is added to that team.

When there's no `elvis.config` in the pull request's branch of your repo a default
set of rules are run. The list of all the rules that are part of this set can be
found [here](https://github.com/inaka/elvis/wiki/Default-Rules-in-Elvis-Webhook).

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
  [elvis-web]: http://elvis.inakalabs.com/
