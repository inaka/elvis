# Contribute

Before anything else... Thank you for your time and contributions!

## Issues (New Features, Rules & Bug Reports)

If you have a feature or rule you want to see added to Elvis, please create an
[issue](https://github.com/inaka/elvis/issues) before doing any work, so that we can discuss it and figure
out what to do about it.

In the case of rules, keep in mind you can always create your [User Defined Rules](https://github.com/inaka/elvis#user-defined-rules), in order to run them locally.

When you find a bug in one of Elvis's rules or the way Elvis itself work, please provide detailed steps
that show how to reproduce the problem.

## Rules

When an issue is already marked with the `rule` label and you are interested in working on it, first check
there is no existing [branch](https://github.com/inaka/elvis/branches) with the issue's number, If so, you
can go ahead and start working on it.

The following is a checklist you can follow when implementing a new Elvis rule:

- [ ] Write both test cases that fail and that pass the rule.
- [ ] Edit the [Rules](https://github.com/inaka/elvis/wiki/Rules) wiki page, by adding the rule to the list.
- [ ] When the issue has the label `default rule`, add the rule to [Default Rules in Elvis Webhook](https://github.com/inaka/elvis/wiki/Default-Rules-in-Elvis-Webhook) and in the [`config/elvis.config`](https://github.com/inaka/elvis/blob/master/config/elvis.config) file.

## Questions?

If you have any questions or general comments regarding how to contribute, please use our public
Erlanger Slack channel: [#elvis](https://erlanger.slack.com/archives/C01073W0E15).
