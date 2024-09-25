# `elvis` [![CI][build-badge]][elvis]

[elvis]: https://github.com/inaka/elvis
[build-badge]: https://github.com/inaka/elvis/workflows/build/badge.svg

![Elvis Presley dancing](https://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

`elvis`, the Erlang style reviewer, is the command-line interface for
[`elvis_core`](https://github.com/inaka/elvis_core).

## What is `elvis`?

`elvis` is an Erlang generic style reviewer that focuses on code and configuration consistency,
as well as readability, across your whole code base. By the very nature of the rules applied from
`elvis_core` it can also be considered a learning tool, by trying to generalize good practices and
allowing teams to adapt it to their own specific needs, while fostering discussions around code
conventions.

### Advantages of using it

Some of the advantages of using `elvis` are:

* enabling consistency in style across all your code base
* encouraging the development team to sit down and talk about code conventions
* allowing continuous monitoring of code quality
* helping developers avoid repeated mistakes that can be automatically detected
* providing homogenisation among different projects in a company, therefore facilitating project
switching for developers, as well as allowing easier code sharing between projects
* learning, since some of the conventions it proposes are themselves the distilled result of
working in very large code bases

## Installation

To use `elvis` as a standalone tool, you need to:

```console
git clone https://github.com/inaka/elvis
cd elvis
rebar3 escriptize
export PATH=${PWD}/_build/default/bin:${PATH}
```

(to make the env. variable export more permanent add it to your shell's configuration file,
i.e. `.bashrc`, `.zshrc`, ...)

Now run it by calling `elvis` (or `elvis help`), and you should get to the `Usage: elvis`
instructions.

### Shell completion

Elvis also comes with shell completions.

Optionally, download and install:

- Bash completion from <https://github.com/inaka/elvis/raw/master/priv/bash_completion/elvis>
- Zsh completion from <https://github.com/inaka/elvis/master/priv/zsh_completion/_elvis>

depending on your preferred shell.

## Usage

The most common use case is to `cd` into a folder containing an `elvis.config` file
and executing `elvis rock`.

If you just execute `elvis` with no arguments or options you'll get to the usage instructions
outlined in this README.

## Options

While you can get a more complete list of options by executing `elvis help`, we try to keep them
documented below.

### `--code-path <dir>` (`-p <dir>`)

Adds `<dir>` to the analysis' code path.

### `--commands`

Outputs the list of commands under stood by `elvis`.

#### `git-branch <branch | commit>`

Executes `elvis` on source files that have changed since `<branch>` or `<commit>`.

#### `git-hook`

Executes `elvis` (with the specific configuration file) on the pre-commit hook Git-staged files.

#### `install git-hook`

Installs `elvis` in your current Git repository, as a pre-commit hook.

#### `rock [file...]`

Executes `elvis` analysis on identified files. It will, by default, consider all the files
in the configuration (i.e. either `elvis.config` or the path set by option `--config`).

### `--config <file>` (`-c <file>`)

Allows providing the path to the config. file (by default `elvis.config` is assumed).

### `--help` (`-h`)

Shows help information.

### `--keep-rocking` (`-k`)

Doesn't stop analysis when erroring out on a file, if given a list of files to analyse.

### `--output-format <plain | colors | parsable>`

Allows controlling the output format of the analysis' results.

The default value is `colors`.

`plain` will output results without colors.
`parsable` will allow for consumption by systems (it's less readable for humans).

### `--parallel <n | auto>` (`-P <n | auto>`)

Allows analyzing files concurrently.

Use `n` to set the desired number of parallel workers, or `auto` to have the application choose
an appropriate value (based on the number of schedulers).

### `--quiet` (`-q`)

Allows suppressing all output. The exit code will still be non-`0` if there are failing rules.

### `--verbose` (`-V`)

Allows verbose output.

### `--version` (`-v`)

Outputs the application's version.

## Configuration

`elvis` is configured via `elvis_core`'s `elvis.config` as detailed under
[`elvis_core / Configuration`](https://github.com/inaka/elvis_core?tab=readme-ov-file#configuration).

### Rules

A reference of all rules implemented in `elvis` can be found in `elvis_core`'s [RULES.md](https://github.com/inaka/elvis_core/blob/main/RULES.md).

### User-defined rules

If you have implemented `elvis` rule that are in your local repository or in one of
your dependencies, you can add these rule to your `elvis.config` file and
tell `elvis` where to find the `.beam` that contains the compiled rule using
the `--code-path` option.

For example, if the rule is in one of your dependencies, you can run `elvis rock -p deps/elvis_rules/ebin -c elvis.config`.

## As a Git hook

`elvis` can be used as a [`git` pre-commit hook](https://git-scm.com/book/en/Customizing-Git-Git-Hooks#Client-Side-Hooks)
using the `git-hook` command (installable via `install git-hook`) as:

```sh
#!/bin/sh
elvis git-hook
```

This will have `elvis` execute on staged files, as per its configuration.

If any rule fails, `elvis` exits with a non-zero code, which signals to `git` that the commit
shouldn't be made.

**Note**: your pre-commit hook script should be executable (i.e. by running
`chmod +x .git/hooks/pre-commit`), otherwise `git` won't be able to execute it.

## As a webhook

`elvis` can be used as a GitHub [webhook](https://developer.github.com/v3/repos/hooks/) for
`pull request` (PR) events, by calling the `elvis_webhook:event/1` function. This will add
a comment in each file and rule that is broken, analyzing only the files associated with the PR.

Since GitHub's API needs a valid username and password to allow the creation of
reviews on PRs, parameters `github_user` and `github_password` need to be
added to `elvis`'s configuration file (mind you that the credentials used
must be from an admin. of the repo or someone with permissions for requesting changes
to PRs).

The `elvis_webhook:event/1` function takes a map containing the keys `headers` and `body`,
whose values should be the map of headers and the body from the GitHub's event
request.

```erlang
Headers = #{<<"X-GitHub-Event">>, <<"pull_request">>},
Body = <<"{}">>, %% JSON data from GitHub's event.
Request = #{headers => Headers, body => Body},
elvis:webhook(Request).
```

The extension to the configuration is as follows:

```erlang
[
  {elvis, [
    {config, [...]},
    %% webhook configuration parameters
    {github_user, "user"},
    {github_password, "password"}
  ]}
].
```

## Documentation

You can generate local documentation with `rebar3 ex_doc` and then access it with `open doc/index.html`.

## Contributing

`elvis` is a FOSS application, and as such contributions are welcome. Be sure to read
the [contributing guide](CONTRIBUTING.md) for more detailed information.

## License

`elvis` is licensed under the [Apache License, Version 2.0](LICENSE).

## Inspiration

`elvis` got some of its inspiration from [HoundCI](https://houndci.com/).
