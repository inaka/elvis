# Change Log

All notable changes to this project will be documented in this file.

## 0.2.5 - TBD

### Added

- [#195] elvis_webhook as an egithub_webhook
- [#185] Rule to enforce dependency url fetch protocol

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#194] Ensure that 'elvis git-hook' does not check deleted files

---

## 0.2.4 - 2015-01-05

### Added

- [#100] Allow user defined rules to be run from the escript.
- [#127] Fulfill the open-source checklist.
- [#142] Only return failures when running elvis:rock/1 from the shell.
- [#148] The god modules rule needs exceptions.
- [#149] Add exceptions to no_deps_master_rebar and no_deps_erlankg_mk.
- [#150] Use ktn_code and delete duplicated code from elvis_code.
- [#154] Documentation for all Elvis rules.
- [#160] Have all rules take a map as the configuration parameter.

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#179] Problems with printing UTF in comments.

---

## 0.2.3 - 2014-10-16

### Added

- [#140] Update version of erlang-github dependency to 0.1.1.

### Deprecated

- None.

### Removed

- None.

### Fixed

- None.

---

## 0.2.2 - 2014-09-26

### Added

- [#87] Use inaka/erlang-github as a dependency.

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#133] Wrong detection of macro as function name with ?MODULE.
- [#130] Reports of lines that include ~p.
- [#122] Webhook error: undef while applying rule used_ignored_variable.
- [#120] Elvis webhook is not working.

---

## 0.2.1 - 2014-09-15

### Added

- [#107] Improve error handling and reporting.

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#101] git-hook is not detecting files that are not erlang modules.
- [#112] Unhandled abstract form error on OTP stdlib.
- [#113] Error when source file contains non ASCII chars.

---

## 0.2.0 - 2014-09-11

### Added

- [#21] Rule: `state` records.
- [#23] Rule: Records in Specs.
- [#103] Rule: Don't use old configuration format.

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#105] Update README with new configuration format.
- [#108] Elvis tries to parse all files, even non erlang ones.

---

## 0.1.1 - 2014-09-09

### Added

- [#1] Initial App Structure.
- [#2] Erlang console interface.
- [#3] Command line interface.
- [#4] git commit integration.
- [#5] GitHub integration.
- [#10] Rule: Macro Names.
- [#11] Rule: Macros in module/function names.
- [#12] Rule: Operator Spaces.
- [#14] Rule: Nesting Level.
- [#16] Rule: God Modules.
- [#17] Rule: Dynamic Functions.
- [#19] Rule: Namespace Naming convention.
- [#20] Rule: Use `callback` not `behaviour_info`.
- [#26] Rule: No `if` expressions.
- [#52] Handle <<"ping">> GitHub event.
- [#59] Access file contents using GitHub API.
- [#47] webhook: read and use the elvis.config from the repository.
- [#64] Improve running time by caching file information.
- [#58] GitHub API: Use OAuth instead of basic authentication.
- [#8] Rules wiki page.
- [#91] make compatible with erlang.mk.
- [#86] Improve feedback when using Elvis on the command line.
- [#18] Rule: no dep in master.

### Deprecated

- None.

### Removed

- None.

### Fixed

- [#34] README: Outdated configuration argument value.
- [#39] Missing link in README.
- [#41] macro_names rule complains about "STAGED_CONTENT(Path)".
- [#42] operator_spaces rule complains about missing spaces inside strings.
- [#56] Comment position has to be relative to the diff.
- [#68] Error in webhook when elvis.config in repo missing.
- [#70] Aleppo library is missing when creating a release
- [#75] Elvis can't find header files.
- [#76] Elvis crashes in `erl_parse:abstract/2`.
- [#84] Elvis crashes when running on processone/ejabberd.
- [#89] ?MODULE should be allowed in the macro_module_names rule.
- [#88] Improve module name default Regex.
- [#93] Elvis/Aleppo can't handle a comment on first line of a module.
- [#92] `used_ignored_variable` reports unused ignored variables.
