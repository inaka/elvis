# Changelog

⚠️ this file is no longer maintained. For the latest releases refer to
the [GitHub releases](https://github.com/inaka/elvis/releases) page.

## [1.0.1](https://github.com/inaka/elvis/tree/1.0.1) (2021-03-24)

[Full Changelog](https://github.com/inaka/elvis/compare/1.0.0...1.0.1)

**Implemented enhancements:**

- DRY checks over modules [\#299](https://github.com/inaka/elvis/issues/299)

**Fixed bugs:**

- There's no ignore option for the `state\_record\_and\_type` rule [\#551](https://github.com/inaka/elvis/issues/551)

**Closed issues:**

- Try to have a GitHub Action for elvis [\#557](https://github.com/inaka/elvis/issues/557)
- Move from Travis CI to GitHub Actions? [\#555](https://github.com/inaka/elvis/issues/555)
- \[META\] Reorganize issues / projects / wikis [\#553](https://github.com/inaka/elvis/issues/553)
- Document new rule atom\_naming\_convention [\#527](https://github.com/inaka/elvis/issues/527)
- Document new macro\_names options [\#526](https://github.com/inaka/elvis/issues/526)
- rule idea: elvis should work on the real code as well [\#500](https://github.com/inaka/elvis/issues/500)
- Elvis should suggest replacing throw with ct:fail [\#394](https://github.com/inaka/elvis/issues/394)
- Detect when a better function should be used [\#184](https://github.com/inaka/elvis/issues/184)
- Executable Path Length [\#24](https://github.com/inaka/elvis/issues/24)

**Merged pull requests:**

- Approach OTP 24 [\#560](https://github.com/inaka/elvis/pull/560) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Fix broken yml for GitHub Actions [\#559](https://github.com/inaka/elvis/pull/559) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Replace Travis CI with GitHub Actions [\#558](https://github.com/inaka/elvis/pull/558) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Wiki-provoked changes [\#556](https://github.com/inaka/elvis/pull/556) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))

## [1.0.0](https://github.com/inaka/elvis/tree/1.0.0) (2020-11-23)

[Full Changelog](https://github.com/inaka/elvis/compare/0.5.0...1.0.0)

**Implemented enhancements:**

- Add a -elvis attribute [\#529](https://github.com/inaka/elvis/issues/529)
- Add ignore option for all existing rules [\#507](https://github.com/inaka/elvis/issues/507)
- \[Q\] Ignore existing issues, without ignoring whole modules. [\#497](https://github.com/inaka/elvis/issues/497)
- Try to implement more M:F/A ignore rules [\#488](https://github.com/inaka/elvis/issues/488)

**Fixed bugs:**

- Prevent hackney compilation warnings on OTP 23 [\#544](https://github.com/inaka/elvis/issues/544)
- The way globs in elvis.config are interpreted generates duplicated analysis of files [\#508](https://github.com/inaka/elvis/issues/508)
- Crash reviewing rebar.config deps with pkg\_name [\#395](https://github.com/inaka/elvis/issues/395)

**Closed issues:**

- elvis\_style:is\_otp\_module only checks `behavior` attribute, not `behaviour` [\#550](https://github.com/inaka/elvis/issues/550)
- elvis\_core link in readme not working [\#545](https://github.com/inaka/elvis/issues/545)
- Discuss whether seemingly text-oriented rules should move to a new API [\#542](https://github.com/inaka/elvis/issues/542)
- Document use of `beam\_files` [\#541](https://github.com/inaka/elvis/issues/541)
- Document use of generic `ignore` option [\#540](https://github.com/inaka/elvis/issues/540)
- New Rule: avoid high number of export\_types [\#539](https://github.com/inaka/elvis/issues/539)
- New Rule: Consistency in andalso/orelse v. ;/, in guards [\#538](https://github.com/inaka/elvis/issues/538)
- Document `-elvis` attribute [\#535](https://github.com/inaka/elvis/issues/535)
- Dynamic call in `try ... of ... catch` not detected as such [\#532](https://github.com/inaka/elvis/issues/532)
- New Rule: Define ChildSpecs in either supervisor or worker [\#531](https://github.com/inaka/elvis/issues/531)
- Document @since in the Rules' Wiki [\#528](https://github.com/inaka/elvis/issues/528)
- Use of defaults in code [\#525](https://github.com/inaka/elvis/issues/525)
- Wiki and/or code issues [\#523](https://github.com/inaka/elvis/issues/523)
- Potential rule duplication [\#522](https://github.com/inaka/elvis/issues/522)
- Release request for `elvis\_core` [\#520](https://github.com/inaka/elvis/issues/520)
- New Rule: atom\_naming\_convention [\#516](https://github.com/inaka/elvis/issues/516)
- Turn macro names into a naming conventions rule [\#515](https://github.com/inaka/elvis/issues/515)
- Var exposed from the scope without return [\#502](https://github.com/inaka/elvis/issues/502)
- elvis\_core violates OTP naming conventions [\#491](https://github.com/inaka/elvis/issues/491)
- Outdated Docs [\#489](https://github.com/inaka/elvis/issues/489)
- Do not use timer:sleep on tests [\#479](https://github.com/inaka/elvis/issues/479)
- Rule idea: check map field updates [\#339](https://github.com/inaka/elvis/issues/339)
- Rule idea: safe binary deserialization [\#306](https://github.com/inaka/elvis/issues/306)
- Rule idea: application name as module prefix [\#304](https://github.com/inaka/elvis/issues/304)
- Add Meta Testing [\#303](https://github.com/inaka/elvis/issues/303)
- Reject use of export\_all [\#290](https://github.com/inaka/elvis/issues/290)
- Detect and enforce comma placement style [\#276](https://github.com/inaka/elvis/issues/276)
- Check for unused fields in records [\#117](https://github.com/inaka/elvis/issues/117)

**Merged pull requests:**

- Bump Version to 1.0.0 [\#552](https://github.com/inaka/elvis/pull/552) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Remove unreachable stuff [\#548](https://github.com/inaka/elvis/pull/548) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Prevent eunit issues + Rock with Elvis [\#547](https://github.com/inaka/elvis/pull/547) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Prevent hackney warnings on future deprecated functions [\#546](https://github.com/inaka/elvis/pull/546) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Prevent a minor warning \(and prepare for the future\) [\#543](https://github.com/inaka/elvis/pull/543) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Simplify defaults in .app.src [\#524](https://github.com/inaka/elvis/pull/524) ([paulo-ferraz-oliveira](https://github.com/paulo-ferraz-oliveira))
- Add non-empty default configuration [\#521](https://github.com/inaka/elvis/pull/521) ([dmitrivereshchagin](https://github.com/dmitrivereshchagin))

## [0.5.0](https://github.com/inaka/elvis/tree/0.5.0) (2020-07-10)

[Full Changelog](https://github.com/inaka/elvis/compare/0.4.2...0.5.0)

**Implemented enhancements:**

- Implement a "confidence score" on each thing Elvis finds [\#138](https://github.com/inaka/elvis/issues/138)
- Standardize output and make it parseable [\#169](https://github.com/inaka/elvis/issues/169)

**Fixed bugs:**

- FUNCTION\_NAME macro treated as variable [\#505](https://github.com/inaka/elvis/issues/505)
- Master doesn't build [\#503](https://github.com/inaka/elvis/issues/503)
- operator\_spaces handles +,- and / signs as operator in cases where it is not an operator [\#483](https://github.com/inaka/elvis/issues/483)
- Elvis is not detecting "Missing space" between a comma [\#376](https://github.com/inaka/elvis/issues/376)
- Crash elvis on eval formatted string [\#288](https://github.com/inaka/elvis/issues/288)
- Fix dialyzer warnings on OTP21 [\#486](https://github.com/inaka/elvis/issues/486)

**Closed issues:**

- Compiling the elvis shell Error [\#506](https://github.com/inaka/elvis/issues/506)
- Possible problems with state\_record\_and\_type validation [\#490](https://github.com/inaka/elvis/issues/490)
- Make corrections for detected problems [\#478](https://github.com/inaka/elvis/issues/478)
- Elvis complains about dynamic function call in behaviour module [\#467](https://github.com/inaka/elvis/issues/467)
- Elvis seems to ignore -ifdef/-else/-endif conditionals and flags disabled code parts as DRY of enabled ones. [\#321](https://github.com/inaka/elvis/issues/321)
- Change of behavior regarding defines? [\#512](https://github.com/inaka/elvis/issues/512)
- Verify dependencies are up to date [\#494](https://github.com/inaka/elvis/issues/494)
- Create elvis.config.sample file [\#438](https://github.com/inaka/elvis/issues/438)
- Rename libraries to match repo names [\#425](https://github.com/inaka/elvis/issues/425)
- Properly document elvis.config [\#380](https://github.com/inaka/elvis/issues/380)
- Create release once ongoing tasks are merged into master [\#287](https://github.com/inaka/elvis/issues/287)
- Create a stable tag/release in order to have a fixed reference to the latest stable version [\#205](https://github.com/inaka/elvis/issues/205)

**Merged pull requests:**

- Prepare release 0.5.0 [\#514](https://github.com/inaka/elvis/pull/514) ([jfacorro](https://github.com/jfacorro))
- README.md code block formatting for Erlang [\#513](https://github.com/inaka/elvis/pull/513) ([szTheory](https://github.com/szTheory))
- \[\#491\] Update elvis\_core with correct OTP naming [\#511](https://github.com/inaka/elvis/pull/511) ([jfacorro](https://github.com/jfacorro))
- \[\#503\] Update egithub [\#504](https://github.com/inaka/elvis/pull/504) ([jfacorro](https://github.com/jfacorro))
- Add 'parallel' option, that runs analysis of files in parallel [\#499](https://github.com/inaka/elvis/pull/499) ([define-null](https://github.com/define-null))
- Add git-branch option for elvis escript to readme [\#496](https://github.com/inaka/elvis/pull/496) ([onno-vos-dev](https://github.com/onno-vos-dev))

## [0.4.2](https://github.com/inaka/elvis/tree/0.4.2) (2018-07-02)

[Full Changelog](https://github.com/inaka/elvis/compare/0.4.1...0.4.2)

**Implemented enhancements:**

- Update elvis.config so that it can be checked with gadget [\#324](https://github.com/inaka/elvis/issues/324)

**Fixed bugs:**

- Misuse of options [\#468](https://github.com/inaka/elvis/issues/468)
- elvis.config not honored for the elvis bot [\#439](https://github.com/inaka/elvis/issues/439)
- Webhook: elvis.config not found when creating PR from fork [\#311](https://github.com/inaka/elvis/issues/311)

**Closed issues:**

- Would you be interested in a Common Caveats rule? [\#484](https://github.com/inaka/elvis/issues/484)
- Bump elvis\_core Version to 0.4.0 [\#482](https://github.com/inaka/elvis/issues/482)
- Update Dependencies on elvis\_core [\#481](https://github.com/inaka/elvis/issues/481)
- Parsing eunit macro ?debugVal fails with Error: function\_clause [\#480](https://github.com/inaka/elvis/issues/480)
- Update elvis\_core dependency version to 0.3.9 [\#470](https://github.com/inaka/elvis/issues/470)
- Bump elvis\_core version to 0.3.9 [\#469](https://github.com/inaka/elvis/issues/469)
- Adding Travis [\#456](https://github.com/inaka/elvis/issues/456)
- elvis.config file documentation [\#437](https://github.com/inaka/elvis/issues/437)
- Fix elvis\_webhook:handle\_pull\_request/3 spec [\#358](https://github.com/inaka/elvis/issues/358)
- Document group level ignore [\#357](https://github.com/inaka/elvis/issues/357)
- There is not elvis:webhook/1 function anymore so please update the documentation [\#322](https://github.com/inaka/elvis/issues/322)

**Merged pull requests:**

- upgrade deps: elvis\_core, katana\_code [\#485](https://github.com/inaka/elvis/pull/485) ([f3c0](https://github.com/f3c0))
- \[\#380\] Add a section describing elvis.config [\#477](https://github.com/inaka/elvis/pull/477) ([JohanVikman](https://github.com/JohanVikman))
- \[\#358\] Fix webhook function spec [\#475](https://github.com/inaka/elvis/pull/475) ([JohanVikman](https://github.com/JohanVikman))
- \[\#322\] Fix webhook readme instructions [\#474](https://github.com/inaka/elvis/pull/474) ([JohanVikman](https://github.com/JohanVikman))
- Update README.md [\#473](https://github.com/inaka/elvis/pull/473) ([igaray](https://github.com/igaray))
- Bump version to 0.4.2 [\#472](https://github.com/inaka/elvis/pull/472) ([harenson](https://github.com/harenson))
- \[Fix \#470\] Update elvis\_core dependency version to 0.3.9 [\#471](https://github.com/inaka/elvis/pull/471) ([harenson](https://github.com/harenson))
- \[\#456\] Adding Travis [\#461](https://github.com/inaka/elvis/pull/461) ([ferigis](https://github.com/ferigis))

## [0.4.1](https://github.com/inaka/elvis/tree/0.4.1) (2017-07-17)

[Full Changelog](https://github.com/inaka/elvis/compare/0.4.0...0.4.1)

**Fixed bugs:**

- Update egithub dependency to version 0.5.2 [\#463](https://github.com/inaka/elvis/issues/463)

**Closed issues:**

- Bump version to 0.4.1 [\#464](https://github.com/inaka/elvis/issues/464)

**Merged pull requests:**

- \[Fix \#464\] Bump version to 0.4.1 [\#466](https://github.com/inaka/elvis/pull/466) ([harenson](https://github.com/harenson))
- \[Fix \#463\] Update egithub dependency to version 0.5.2 [\#465](https://github.com/inaka/elvis/pull/465) ([harenson](https://github.com/harenson))

## [0.4.0](https://github.com/inaka/elvis/tree/0.4.0) (2017-07-06)

[Full Changelog](https://github.com/inaka/elvis/compare/0.3.4...0.4.0)

**Implemented enhancements:**

- Use the new PR review feature [\#449](https://github.com/inaka/elvis/issues/449)

**Closed issues:**

- Bump version to 0.4.0 [\#458](https://github.com/inaka/elvis/issues/458)

**Merged pull requests:**

- \[Fix \#458\] Bump version to 0.4.0 [\#460](https://github.com/inaka/elvis/pull/460) ([harenson](https://github.com/harenson))
- \[Fix \#449\] Use the new GitHub's PR review feature [\#457](https://github.com/inaka/elvis/pull/457) ([harenson](https://github.com/harenson))

## [0.3.4](https://github.com/inaka/elvis/tree/0.3.4) (2017-06-22)

[Full Changelog](https://github.com/inaka/elvis/compare/0.3.3...0.3.4)

**Fixed bugs:**

- Elvis is failing badly when operator\_spaces and line\_length checks don't pass [\#450](https://github.com/inaka/elvis/issues/450)

**Closed issues:**

- Bump version to 0.3.4 [\#454](https://github.com/inaka/elvis/issues/454)
- Update elvis\_core dependency to 0.3.8 version [\#452](https://github.com/inaka/elvis/issues/452)
- Bump elvis\_core version to 0.3.7 and hex.pm it! [\#451](https://github.com/inaka/elvis/issues/451)
- Add `--verbose` flag, obey rule of silence of it's not passed [\#447](https://github.com/inaka/elvis/issues/447)

**Merged pull requests:**

- \[Fix \#454\] Bump version to 0.3.4 [\#455](https://github.com/inaka/elvis/pull/455) ([harenson](https://github.com/harenson))
- \[Fix \#452\] Upgrade elvis\_core dependency to 0.3.8 and remove unused jiffy stuff [\#453](https://github.com/inaka/elvis/pull/453) ([harenson](https://github.com/harenson))
- Add `--verbose` flag, setting verbose app env option [\#448](https://github.com/inaka/elvis/pull/448) ([srenatus](https://github.com/srenatus))

## [0.3.3](https://github.com/inaka/elvis/tree/0.3.3) (2017-04-25)

[Full Changelog](https://github.com/inaka/elvis/compare/0.3.2...0.3.3)

**Fixed bugs:**

- Implicit funs shouldn't be counted as a level. [\#342](https://github.com/inaka/elvis/issues/342)

**Closed issues:**

- Bump version to 0.3.3 [\#445](https://github.com/inaka/elvis/issues/445)
- Update elvis\_core dependency [\#443](https://github.com/inaka/elvis/issues/443)
- Bump Version elvis\_core to 0.3.6 [\#442](https://github.com/inaka/elvis/issues/442)
- Change line\_length default to 100 [\#441](https://github.com/inaka/elvis/issues/441)
- Version Bump to 0.3.2 [\#431](https://github.com/inaka/elvis/issues/431)

**Merged pull requests:**

- \[Close \#445\] bump version to 0.3.3 [\#446](https://github.com/inaka/elvis/pull/446) ([Euen](https://github.com/Euen))
- \[Close \#443\] update elvis\_core dependency [\#444](https://github.com/inaka/elvis/pull/444) ([Euen](https://github.com/Euen))
- Exit with non-zero exit code on all failures [\#436](https://github.com/inaka/elvis/pull/436) ([tjarvstrand](https://github.com/tjarvstrand))
- Add --keep-rocking switch [\#435](https://github.com/inaka/elvis/pull/435) ([fenek](https://github.com/fenek))

## [0.3.2](https://github.com/inaka/elvis/tree/0.3.2) (2017-02-16)

[Full Changelog](https://github.com/inaka/elvis/compare/0.3.1...0.3.2)

**Closed issues:**

- Update egithub dependency [\#432](https://github.com/inaka/elvis/issues/432)
- Version Bump to 0.3.1 [\#429](https://github.com/inaka/elvis/issues/429)

**Merged pull requests:**

- \[\#431\] version bump to 0.3.2 [\#434](https://github.com/inaka/elvis/pull/434) ([ferigis](https://github.com/ferigis))
- \[\#432\] updating egithub dependency [\#433](https://github.com/inaka/elvis/pull/433) ([ferigis](https://github.com/ferigis))

## [0.3.1](https://github.com/inaka/elvis/tree/0.3.1) (2017-02-16)

[Full Changelog](https://github.com/inaka/elvis/compare/0.3.0...0.3.1)

**Merged pull requests:**

- \[\#429\] Bump Version to 0.3.1 [\#430](https://github.com/inaka/elvis/pull/430) ([ferigis](https://github.com/ferigis))

## [0.3.0](https://github.com/inaka/elvis/tree/0.3.0) (2017-02-10)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.12...0.3.0)

**Implemented enhancements:**

- Remove lager as a dep from elvis\_core since it is practically unused [\#318](https://github.com/inaka/elvis/issues/318)

**Fixed bugs:**

- max\_function\_length check fails unexpectedly [\#407](https://github.com/inaka/elvis/issues/407)
- Binary package for latest release [\#393](https://github.com/inaka/elvis/issues/393)
- Link to elvis v0.2.12 points to the wrong version [\#419](https://github.com/inaka/elvis/issues/419)
- Fix dialyzer warnings [\#406](https://github.com/inaka/elvis/issues/406)

**Closed issues:**

- Allow Elvis to run on all changes in a particular branch [\#420](https://github.com/inaka/elvis/issues/420)
- Bump version to 0.3.4 [\#417](https://github.com/inaka/elvis/issues/417)
- Fix wrong type [\#416](https://github.com/inaka/elvis/issues/416)
- Update elvis\_core dep version to 0.3.4 [\#415](https://github.com/inaka/elvis/issues/415)
- Bump Version for elvis\_core to 0.3.3 [\#414](https://github.com/inaka/elvis/issues/414)
- Style rules for the seqbind transform [\#413](https://github.com/inaka/elvis/issues/413)
- Check fails on eunit tests [\#403](https://github.com/inaka/elvis/issues/403)
- Get the project to compile with OTP19.x [\#399](https://github.com/inaka/elvis/issues/399)
- Use https in URLs when setting new webhook [\#397](https://github.com/inaka/elvis/issues/397)
- Migrate existing webhooks to use https instead of http in their URLs [\#396](https://github.com/inaka/elvis/issues/396)
- New Hex release [\#375](https://github.com/inaka/elvis/issues/375)
- Update dependencies [\#424](https://github.com/inaka/elvis/issues/424)
- Bump Versions [\#423](https://github.com/inaka/elvis/issues/423)
- Fix tests [\#418](https://github.com/inaka/elvis/issues/418)
- Default rulesets doesn't include no\_nested\_try\_catch rule [\#410](https://github.com/inaka/elvis/issues/410)
- Include ignore\_functions rule doc for max\_function\_length rule [\#408](https://github.com/inaka/elvis/issues/408)
- Add ct:print on elvis\_style:no\_debug\_call default functions [\#402](https://github.com/inaka/elvis/issues/402)

**Merged pull requests:**

- \[\#423\] Version Bump to 0.3.0 [\#428](https://github.com/inaka/elvis/pull/428) ([ferigis](https://github.com/ferigis))
- Add option for running Elvis on all changes since branch or commit [\#421](https://github.com/inaka/elvis/pull/421) ([onno-vos-dev](https://github.com/onno-vos-dev))
- Remove Lager as a direct dependency of Elvis [\#411](https://github.com/inaka/elvis/pull/411) ([waisbrot](https://github.com/waisbrot))
- Add link to GitHub API page [\#405](https://github.com/inaka/elvis/pull/405) ([mzaini30](https://github.com/mzaini30))
- Allow filenames on command line [\#401](https://github.com/inaka/elvis/pull/401) ([bartekgorny](https://github.com/bartekgorny))
- \[Close \#399\] Update lager [\#400](https://github.com/inaka/elvis/pull/400) ([Euen](https://github.com/Euen))
- Add getopt to applications [\#398](https://github.com/inaka/elvis/pull/398) ([mururu](https://github.com/mururu))
- \[\#419\] link to correct version [\#427](https://github.com/inaka/elvis/pull/427) ([ferigis](https://github.com/ferigis))
- \[\#424\] updating dependencies [\#426](https://github.com/inaka/elvis/pull/426) ([ferigis](https://github.com/ferigis))

## [0.2.12](https://github.com/inaka/elvis/tree/0.2.12) (2016-08-05)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.11...0.2.12)

**Fixed bugs:**

- Missing builds for elvis.mk [\#373](https://github.com/inaka/elvis/issues/373)
- operator spaces rules fails in lines with a unicode char [\#343](https://github.com/inaka/elvis/issues/343)
- variable\_naming\_convention and MACROs [\#362](https://github.com/inaka/elvis/pull/362) ([andreineculau](https://github.com/andreineculau))

**Closed issues:**

- Version Bump to 0.2.12 [\#391](https://github.com/inaka/elvis/issues/391)
- Version Bump to 0.3.2 [\#389](https://github.com/inaka/elvis/issues/389)
- Version Bump to 0.3.1 [\#388](https://github.com/inaka/elvis/issues/388)
- Move from erlang.mk to rebar3 [\#387](https://github.com/inaka/elvis/issues/387)
- Version Bump to 0.3.0 [\#386](https://github.com/inaka/elvis/issues/386)
- Please update lager [\#385](https://github.com/inaka/elvis/issues/385)
- Function clause when rule is disabled [\#384](https://github.com/inaka/elvis/issues/384)
- Add Inaka Logo in every page [\#383](https://github.com/inaka/elvis/issues/383)
- Move from erlang.mk to rebar3 [\#381](https://github.com/inaka/elvis/issues/381)
- BUG: Files in directory '.' are filtered out, so never checked [\#374](https://github.com/inaka/elvis/issues/374)

**Merged pull requests:**

- \[Close \#391\] version bump 0.2.12 [\#392](https://github.com/inaka/elvis/pull/392) ([Euen](https://github.com/Euen))
- Euen.387.rebar3 [\#390](https://github.com/inaka/elvis/pull/390) ([Euen](https://github.com/Euen))
- Update README.md [\#377](https://github.com/inaka/elvis/pull/377) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.11](https://github.com/inaka/elvis/tree/0.2.11) (2016-04-07)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.10...0.2.11)

**Closed issues:**

- Update katana\_code to 0.0.3 in elvis\_core [\#371](https://github.com/inaka/elvis/issues/371)

**Merged pull requests:**

- Version Bump to 0.2.11 [\#372](https://github.com/inaka/elvis/pull/372) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.10](https://github.com/inaka/elvis/tree/0.2.10) (2016-03-30)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.8...0.2.10)

**Fixed bugs:**

- elvis\_core specifies aleppo as a dep but downloads inaka\_aleppo [\#367](https://github.com/inaka/elvis/issues/367)

**Closed issues:**

- Get elvis\_core to use katana\_code 0.0.2 [\#368](https://github.com/inaka/elvis/issues/368)
- Bump version to 0.2.9 \(elvis\_core\) [\#365](https://github.com/inaka/elvis/issues/365)
- Fix specs \(elvis\_core\) [\#364](https://github.com/inaka/elvis/issues/364)
- How to implement my own rules? [\#363](https://github.com/inaka/elvis/issues/363)
- Update elvis\_core repo and make it ready for hex.pm [\#360](https://github.com/inaka/elvis/issues/360)
- Update repo and make it ready for hex.pm [\#359](https://github.com/inaka/elvis/issues/359)
- can I exclude files from the check? [\#356](https://github.com/inaka/elvis/issues/356)
- Update katana dependency in elvis [\#354](https://github.com/inaka/elvis/issues/354)
- Update katana dependency in elvis\_core [\#353](https://github.com/inaka/elvis/issues/353)
- Update erlang-katana dep to 0.2.23 when published and bump version [\#341](https://github.com/inaka/elvis/issues/341)
- read rebar.config elvis section if exists [\#307](https://github.com/inaka/elvis/issues/307)

**Merged pull requests:**

- Fix katana\_code version [\#370](https://github.com/inaka/elvis/pull/370) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Version Bump to 0.2.10 [\#369](https://github.com/inaka/elvis/pull/369) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[Fix \#359\] Update dependencies; Update erlang.mk; Add meta testing [\#366](https://github.com/inaka/elvis/pull/366) ([harenson](https://github.com/harenson))
- \[Fix \#354\] Update katana [\#355](https://github.com/inaka/elvis/pull/355) ([harenson](https://github.com/harenson))

## [0.2.8](https://github.com/inaka/elvis/tree/0.2.8) (2016-03-08)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.7...0.2.8)

**Implemented enhancements:**

- Update for rebar3 [\#280](https://github.com/inaka/elvis/issues/280)

**Closed issues:**

- Bump elvis version to 0.2.8 [\#351](https://github.com/inaka/elvis/issues/351)
- Bump elvis\_core version to 0.2.8-2 [\#349](https://github.com/inaka/elvis/issues/349)
- Remove unused dependency aleppo in elvis\_core [\#347](https://github.com/inaka/elvis/issues/347)
- Remove unused dependency aleppo in elvis [\#346](https://github.com/inaka/elvis/issues/346)
- Bump elvis\_core version to 0.2.8 [\#345](https://github.com/inaka/elvis/issues/345)
- Update to the latest version of elvis\_core [\#344](https://github.com/inaka/elvis/issues/344)
- Trouble using built escript with recent builds. [\#333](https://github.com/inaka/elvis/issues/333)

**Merged pull requests:**

- \[Fix \#351\] Bump version to 0.2.8 [\#352](https://github.com/inaka/elvis/pull/352) ([harenson](https://github.com/harenson))
- \[Fix \#344\] Update elvis\_core dependency version; Update config files [\#350](https://github.com/inaka/elvis/pull/350) ([harenson](https://github.com/harenson))
- \[Fix \#346\] Remove unused application [\#348](https://github.com/inaka/elvis/pull/348) ([harenson](https://github.com/harenson))
- Update README.md [\#340](https://github.com/inaka/elvis/pull/340) ([StoneCypher](https://github.com/StoneCypher))

## [0.2.7](https://github.com/inaka/elvis/tree/0.2.7) (2016-01-22)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.6...0.2.7)

**Implemented enhancements:**

- Update Katana version  [\#332](https://github.com/inaka/elvis/issues/332)

**Fixed bugs:**

- Update katana to 0.2.22 in elvis\_core and bump version to 0.2.7 [\#335](https://github.com/inaka/elvis/issues/335)
- Missing elvis\_core dep in rebar.config [\#330](https://github.com/inaka/elvis/issues/330)

**Closed issues:**

- Bump version to 0.2.7 [\#337](https://github.com/inaka/elvis/issues/337)
- Update katana to 0.2.21 in elvis\_core so that it compiles successfully [\#334](https://github.com/inaka/elvis/issues/334)

**Merged pull requests:**

- \[Close \#337\] Update elvis\_core version to 0.2.7 [\#338](https://github.com/inaka/elvis/pull/338) ([jfacorro](https://github.com/jfacorro))
- \[\#335\] Update elvis\_core version to 0.2.7 [\#336](https://github.com/inaka/elvis/pull/336) ([jfacorro](https://github.com/jfacorro))
- \[Fix \#330\] add elvis\_core 0.2.6 in rebar.config [\#331](https://github.com/inaka/elvis/pull/331) ([Euen](https://github.com/Euen))

## [0.2.6](https://github.com/inaka/elvis/tree/0.2.6) (2016-01-15)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.6-alpha1...0.2.6)

**Implemented enhancements:**

- Remove calls io:format in style\_SUITE [\#326](https://github.com/inaka/elvis/issues/326)
- Splitting out elvis into parts [\#300](https://github.com/inaka/elvis/issues/300)
- Add CONTRIBUTING.md [\#285](https://github.com/inaka/elvis/issues/285)
- file exceptions / global ignore \(config\) [\#190](https://github.com/inaka/elvis/issues/190)
- Extending default configuration. [\#166](https://github.com/inaka/elvis/issues/166)

**Fixed bugs:**

- old\_configuration\_format rule is throwing an error [\#325](https://github.com/inaka/elvis/issues/325)
- Incorrect detecting rule missing space after "++" [\#320](https://github.com/inaka/elvis/issues/320)
- Elvis does not distinguish ++ operator from + [\#319](https://github.com/inaka/elvis/issues/319)
- Incorrect detecting length of unicode strings  [\#301](https://github.com/inaka/elvis/issues/301)
- Incorrect detecting rule missing space after "," [\#296](https://github.com/inaka/elvis/issues/296)
- state\_record\_and\_type false positive when record includes defines from header file [\#295](https://github.com/inaka/elvis/issues/295)
- Elvis github integration fails when renaming files [\#291](https://github.com/inaka/elvis/issues/291)
- Elvis failed `badarg` in inaka/serpents\#74 [\#273](https://github.com/inaka/elvis/issues/273)
- Running elvis compiled with 18.0 results in weird behavior  [\#258](https://github.com/inaka/elvis/issues/258)
- Elvis failed while reviewing your PR: `bad\_key` [\#234](https://github.com/inaka/elvis/issues/234)
- Elvis is failing in the following referenced PR [\#232](https://github.com/inaka/elvis/issues/232)
- Wrong config spec [\#292](https://github.com/inaka/elvis/pull/292) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Version Bump to 0.2.6 [\#328](https://github.com/inaka/elvis/issues/328)
- Version Bump to 0.2.6 at elvis\_core [\#327](https://github.com/inaka/elvis/issues/327)
- Update documentation to include new default configuration feature. [\#323](https://github.com/inaka/elvis/issues/323)
- Update zipper and katana deps to avoid compiler warnings [\#317](https://github.com/inaka/elvis/issues/317)
- Create a release for elvis\_core [\#315](https://github.com/inaka/elvis/issues/315)
- Rename the application `elvis` to `elvis\_shell` [\#314](https://github.com/inaka/elvis/issues/314)
- Rename the application name back to `elvis` in `elvis\_core` [\#313](https://github.com/inaka/elvis/issues/313)
- Add a reference in elvis\_core's README to create issues in elvis repo  [\#312](https://github.com/inaka/elvis/issues/312)
- What's wrong with the documented configuration? [\#309](https://github.com/inaka/elvis/issues/309)
- Rule Idea: Missing spec [\#308](https://github.com/inaka/elvis/issues/308)
- Incorrect detecting length of unicode strings [\#297](https://github.com/inaka/elvis/issues/297)
- Make Elvis a third-party Erlang.mk plugin [\#289](https://github.com/inaka/elvis/issues/289)
- Don't leave debugging calls in your source code [\#270](https://github.com/inaka/elvis/issues/270)
- Variable Names [\#187](https://github.com/inaka/elvis/issues/187)
- Function Names [\#186](https://github.com/inaka/elvis/issues/186)
- No nesting try…catch'es [\#163](https://github.com/inaka/elvis/issues/163)

**Merged pull requests:**

- \[Fix \#328\] update release 0.2.6 [\#329](https://github.com/inaka/elvis/pull/329) ([Euen](https://github.com/Euen))
- \[Closes \#300\] Split elvis into parts [\#316](https://github.com/inaka/elvis/pull/316) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#309\] Update example [\#310](https://github.com/inaka/elvis/pull/310) ([jfacorro](https://github.com/jfacorro))
- Bump lager version to 3.0.2 [\#305](https://github.com/inaka/elvis/pull/305) ([sargun](https://github.com/sargun))
- Missing dependency repository [\#302](https://github.com/inaka/elvis/pull/302) ([luisgabriel](https://github.com/luisgabriel))
- Rebar3 fix [\#294](https://github.com/inaka/elvis/pull/294) ([Licenser](https://github.com/Licenser))
- Fix line length for unicode files [\#293](https://github.com/inaka/elvis/pull/293) ([Licenser](https://github.com/Licenser))
- \[Closes \#285\] CONTRIBUTING.md [\#286](https://github.com/inaka/elvis/pull/286) ([jfacorro](https://github.com/jfacorro))
- \[\#163\] No nesting try…catch'es  [\#284](https://github.com/inaka/elvis/pull/284) ([ElFantasma](https://github.com/ElFantasma))
- Add variable names rule [\#283](https://github.com/inaka/elvis/pull/283) ([harenson](https://github.com/harenson))
- \[\#270\] Implement no\_debug\_call rule. [\#282](https://github.com/inaka/elvis/pull/282) ([bullno1](https://github.com/bullno1))
- Master [\#279](https://github.com/inaka/elvis/pull/279) ([JakeQiu3](https://github.com/JakeQiu3))
- Issue\#186 function names refactoring [\#277](https://github.com/inaka/elvis/pull/277) ([kbaird](https://github.com/kbaird))
- \[Closes \#273\] Add global option to re:replace call [\#275](https://github.com/inaka/elvis/pull/275) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#258\] Updated katana dep which handles changes in OTP 18.0 [\#274](https://github.com/inaka/elvis/pull/274) ([jfacorro](https://github.com/jfacorro))
- Support for Rebar3 style deps [\#269](https://github.com/inaka/elvis/pull/269) ([walrusVision](https://github.com/walrusVision))

## [0.2.6-alpha1](https://github.com/inaka/elvis/tree/0.2.6-alpha1) (2015-09-07)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5...0.2.6-alpha1)

**Fixed bugs:**

- Incorrect matching of files [\#252](https://github.com/inaka/elvis/issues/252)

**Closed issues:**

- 0.2.6-alpha1 release [\#271](https://github.com/inaka/elvis/issues/271)

**Merged pull requests:**

- \[Closes \#271\] Release 0.2.6-alpha1 [\#272](https://github.com/inaka/elvis/pull/272) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#252\] Incorrect matching of files [\#268](https://github.com/inaka/elvis/pull/268) ([jfacorro](https://github.com/jfacorro))

## [0.2.5](https://github.com/inaka/elvis/tree/0.2.5) (2015-08-31)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5-beta4...0.2.5)

**Implemented enhancements:**

- `max\_function\_len` rule should ignore whitespace and comments [\#263](https://github.com/inaka/elvis/issues/263)
- Options for max\_module\_length [\#261](https://github.com/inaka/elvis/issues/261)
- Improve git\_for\_deps\_erlang\_mk and git\_for\_deps\_rebar [\#199](https://github.com/inaka/elvis/issues/199)
- Add --version option [\#174](https://github.com/inaka/elvis/issues/174)
- Add an elvis:rock\_this/1 function to rock specific modules [\#159](https://github.com/inaka/elvis/issues/159)

**Fixed bugs:**

- Possible false positive? [\#236](https://github.com/inaka/elvis/issues/236)
- Missing space after comma inside a string being reported  [\#233](https://github.com/inaka/elvis/issues/233)
- Unhandled abstract form issue with fun\(\(...\)  [\#239](https://github.com/inaka/elvis/issues/239)
- Non spacing not detected [\#221](https://github.com/inaka/elvis/issues/221)

**Closed issues:**

- Release version 0.2.5 [\#266](https://github.com/inaka/elvis/issues/266)
- Order line results by line\_num [\#219](https://github.com/inaka/elvis/issues/219)
- White output [\#209](https://github.com/inaka/elvis/issues/209)
- Don't use spaces for indentation [\#178](https://github.com/inaka/elvis/issues/178)
- Fix the specs on elvis\_style and elvis\_project [\#162](https://github.com/inaka/elvis/issues/162)
- New rule: max\_function\_length [\#153](https://github.com/inaka/elvis/issues/153)
- New rule: max\_module\_length [\#152](https://github.com/inaka/elvis/issues/152)
- No Trailing Whitespace [\#147](https://github.com/inaka/elvis/issues/147)

**Merged pull requests:**

- \[Closes \#266\] Release 0.2.5 [\#267](https://github.com/inaka/elvis/pull/267) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#263\] Options for max function length [\#265](https://github.com/inaka/elvis/pull/265) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#261\] Options for max module line length [\#264](https://github.com/inaka/elvis/pull/264) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#153\] Max function length rule [\#262](https://github.com/inaka/elvis/pull/262) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#152\] Max module length rule [\#260](https://github.com/inaka/elvis/pull/260) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#236\] Operator space false positive [\#259](https://github.com/inaka/elvis/pull/259) ([jfacorro](https://github.com/jfacorro))
- Fix typo in specs [\#257](https://github.com/inaka/elvis/pull/257) ([NOMORECOFFEE](https://github.com/NOMORECOFFEE))

## [0.2.5-beta4](https://github.com/inaka/elvis/tree/0.2.5-beta4) (2015-08-20)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5-beta3...0.2.5-beta4)

**Closed issues:**

- Create bump Version  0.2.5-beta4  [\#255](https://github.com/inaka/elvis/issues/255)
- Error analyzing PR when Github appended this line \<\<"\\ No newline at end of file"\>\> [\#253](https://github.com/inaka/elvis/issues/253)
- The default for git\_for\_deps\_erlang\_mk should be https [\#238](https://github.com/inaka/elvis/issues/238)

**Merged pull requests:**

- Version bump 0.2.5-beta4 [\#256](https://github.com/inaka/elvis/pull/256) ([davecaos](https://github.com/davecaos))
- Added '\' to patch\_line\_type\(\) function [\#254](https://github.com/inaka/elvis/pull/254) ([davecaos](https://github.com/davecaos))
- ibrowse does not build under R18. This is fixed in current ibrowse. [\#251](https://github.com/inaka/elvis/pull/251) ([technion](https://github.com/technion))
- Update Jiffy, as the previous version does not compile on GCC 5.1.0 [\#250](https://github.com/inaka/elvis/pull/250) ([technion](https://github.com/technion))
- Use https instead of git remotes [\#249](https://github.com/inaka/elvis/pull/249) ([guilleiguaran](https://github.com/guilleiguaran))
- \[Close \#238\] change https protocol as default for deps rule [\#248](https://github.com/inaka/elvis/pull/248) ([Euen](https://github.com/Euen))

## [0.2.5-beta3](https://github.com/inaka/elvis/tree/0.2.5-beta3) (2015-06-30)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5-beta2...0.2.5-beta3)

**Fixed bugs:**

- old\_configuration\_format rule not working for PRs [\#244](https://github.com/inaka/elvis/issues/244)
- Not using elvis.config from the PR branch [\#242](https://github.com/inaka/elvis/issues/242)

**Closed issues:**

- Pre-release 0.2.5-beta3 [\#246](https://github.com/inaka/elvis/issues/246)

**Merged pull requests:**

- \[Closes \#246\] Updated changelog [\#247](https://github.com/inaka/elvis/pull/247) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#244\] Use ktn\_code:consult/1 instead of file:consult/1 [\#245](https://github.com/inaka/elvis/pull/245) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#242\] Use the branch from the PR [\#243](https://github.com/inaka/elvis/pull/243) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#239\] Updated katana dep to 0.2.7, add spec with fun\(\(...\) -\> ok\) [\#240](https://github.com/inaka/elvis/pull/240) ([jfacorro](https://github.com/jfacorro))

## [0.2.5-beta2](https://github.com/inaka/elvis/tree/0.2.5-beta2) (2015-06-19)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5-beta...0.2.5-beta2)

**Closed issues:**

- exception error: undefined function [\#230](https://github.com/inaka/elvis/issues/230)
- Instead of using master, use the current branch for elvis.config [\#216](https://github.com/inaka/elvis/issues/216)
- DRY [\#28](https://github.com/inaka/elvis/issues/28)

**Merged pull requests:**

- Version bump to 0.2.5-beta2 [\#237](https://github.com/inaka/elvis/pull/237) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Updated license [\#235](https://github.com/inaka/elvis/pull/235) ([spike886](https://github.com/spike886))
- \[Close \#174\] add version option [\#231](https://github.com/inaka/elvis/pull/231) ([Euen](https://github.com/Euen))
- WIP \[Close \#178\] add no spaces rule [\#229](https://github.com/inaka/elvis/pull/229) ([Euen](https://github.com/Euen))
- \[Fix \#209\] add output-format option to show result without colors [\#228](https://github.com/inaka/elvis/pull/228) ([Euen](https://github.com/Euen))
- Fix bad type [\#227](https://github.com/inaka/elvis/pull/227) ([Euen](https://github.com/Euen))
- \[\#219\] order line results [\#226](https://github.com/inaka/elvis/pull/226) ([Euen](https://github.com/Euen))
- \[Close \#199\] improve protocol for deps [\#225](https://github.com/inaka/elvis/pull/225) ([Euen](https://github.com/Euen))
- \[\#221\] fix bug non spacing not detected [\#223](https://github.com/inaka/elvis/pull/223) ([Euen](https://github.com/Euen))
- \[Fix \#216\] take elvis.config from current branch [\#222](https://github.com/inaka/elvis/pull/222) ([Euen](https://github.com/Euen))
- \[\#28\] Updated zipper functions after renaming edit\_all to fmap [\#220](https://github.com/inaka/elvis/pull/220) ([jfacorro](https://github.com/jfacorro))
- WIP \[\#28\] DRY rule [\#218](https://github.com/inaka/elvis/pull/218) ([jfacorro](https://github.com/jfacorro))

## [0.2.5-beta](https://github.com/inaka/elvis/tree/0.2.5-beta) (2015-04-24)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.5-alpha...0.2.5-beta)

**Implemented enhancements:**

- Investigate erl\_tidy [\#38](https://github.com/inaka/elvis/issues/38)

**Fixed bugs:**

- Ignored variable in spec [\#206](https://github.com/inaka/elvis/issues/206)
- Rebar master deps undetected [\#202](https://github.com/inaka/elvis/issues/202)
- operator\_spaces: fails with $, [\#181](https://github.com/inaka/elvis/issues/181)

**Closed issues:**

- Cloning getopt problem [\#201](https://github.com/inaka/elvis/issues/201)
- Specify source file on the command line [\#170](https://github.com/inaka/elvis/issues/170)
- Don't let modules loose [\#29](https://github.com/inaka/elvis/issues/29)
- Exported vs. Non-exported [\#27](https://github.com/inaka/elvis/issues/27)

**Merged pull requests:**

- Generate 0.2.5-beta release [\#217](https://github.com/inaka/elvis/pull/217) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Add rule checking for trailing whitespace [\#215](https://github.com/inaka/elvis/pull/215) ([dvaergiller](https://github.com/dvaergiller))
- \[Closes \#206\] Ignored variable in spec [\#214](https://github.com/inaka/elvis/pull/214) ([jfacorro](https://github.com/jfacorro))
- \[\#162\] Fixed left behind spec. [\#213](https://github.com/inaka/elvis/pull/213) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#162\] Fix specs [\#212](https://github.com/inaka/elvis/pull/212) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#170\] Test case for the rock\_this/1 function. [\#211](https://github.com/inaka/elvis/pull/211) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#181\] Operator spaces bug [\#210](https://github.com/inaka/elvis/pull/210) ([jfacorro](https://github.com/jfacorro))
- waffle.io Badge [\#208](https://github.com/inaka/elvis/pull/208) ([waffle-iron](https://github.com/waffle-iron))
- Add an option to skip comments when checking line length rule [\#207](https://github.com/inaka/elvis/pull/207) ([kolorahl](https://github.com/kolorahl))
- \[Closes \#202\] Webhook not adding project rules comments [\#204](https://github.com/inaka/elvis/pull/204) ([jfacorro](https://github.com/jfacorro))
- Replaced tildes for dahses since GitGub shows sections as code. [\#203](https://github.com/inaka/elvis/pull/203) ([jfacorro](https://github.com/jfacorro))

## [0.2.5-alpha](https://github.com/inaka/elvis/tree/0.2.5-alpha) (2015-01-14)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.4...0.2.5-alpha)

**Closed issues:**

- Guys, please stop deleting/changing tags [\#198](https://github.com/inaka/elvis/issues/198)
- "elvis git-hook" should not check deleted file [\#193](https://github.com/inaka/elvis/issues/193)

**Merged pull requests:**

- Move to egithub 0.1.5 [\#200](https://github.com/inaka/elvis/pull/200) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix elvis\_webhook according to the new version of egithub\_webhook [\#197](https://github.com/inaka/elvis/pull/197) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Revert "Bypass operator\_spaces when unable to find node type" [\#196](https://github.com/inaka/elvis/pull/196) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Migrate elvis\_webhook [\#195](https://github.com/inaka/elvis/pull/195) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Ensure that 'elvis git-hook' does not check deleted files [\#194](https://github.com/inaka/elvis/pull/194) ([bullno1](https://github.com/bullno1))
- Bypass operator\_spaces when unable to find node type [\#189](https://github.com/inaka/elvis/pull/189) ([andreineculau](https://github.com/andreineculau))
- Rule to enforce dependency url fetch protocol [\#185](https://github.com/inaka/elvis/pull/185) ([igaray](https://github.com/igaray))

## [0.2.4](https://github.com/inaka/elvis/tree/0.2.4) (2015-01-05)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.3...0.2.4)

**Implemented enhancements:**

- New command: install [\#144](https://github.com/inaka/elvis/issues/144)
- Add a make install [\#143](https://github.com/inaka/elvis/issues/143)
- Have all rules take a map as the configuration parameter [\#160](https://github.com/inaka/elvis/issues/160)
- Documentation for all Elvis rules [\#154](https://github.com/inaka/elvis/issues/154)
- Use ktn\_code and delete duplicated code from elvis\_code [\#150](https://github.com/inaka/elvis/issues/150)
- Add exceptions to no\_deps\_master\_rebar and no\_deps\_erlankg\_mk [\#149](https://github.com/inaka/elvis/issues/149)
- The god modules rule needs exceptions [\#148](https://github.com/inaka/elvis/issues/148)
- Only return failures when running elvis:rock/1 from the shell [\#142](https://github.com/inaka/elvis/issues/142)

**Fixed bugs:**

- Think Bin's element with size is a module [\#172](https://github.com/inaka/elvis/issues/172)
- Problems with printing UTF in comments. [\#179](https://github.com/inaka/elvis/issues/179)

**Closed issues:**

- Make commandline 'rock' invocation with FAIL notices return 1 to the shell. [\#175](https://github.com/inaka/elvis/issues/175)
- I think it tried to parse my Makefile [\#168](https://github.com/inaka/elvis/issues/168)
- Erlang.mk Plugin [\#96](https://github.com/inaka/elvis/issues/96)
- Bump to 0.2.4 [\#191](https://github.com/inaka/elvis/issues/191)
- Fulfill the open-source checklist [\#127](https://github.com/inaka/elvis/issues/127)
- Allow user defined rules to be run from the escript [\#100](https://github.com/inaka/elvis/issues/100)
- Indentation Format [\#15](https://github.com/inaka/elvis/issues/15)

**Merged pull requests:**

- \[\#191\] Bump to 0.2.4 [\#192](https://github.com/inaka/elvis/pull/192) ([jfacorro](https://github.com/jfacorro))
- Use git:// instead of ssh remotes [\#188](https://github.com/inaka/elvis/pull/188) ([andreineculau](https://github.com/andreineculau))
- \[Closes \#179\] Correctly print and quote lines with cyrilic chars. [\#180](https://github.com/inaka/elvis/pull/180) ([jfacorro](https://github.com/jfacorro))
- Stricter match for macros as module/function names [\#177](https://github.com/inaka/elvis/pull/177) ([dvaergiller](https://github.com/dvaergiller))
- halt\(1\) after 'elvis rock' invocation fails. [\#176](https://github.com/inaka/elvis/pull/176) ([emauton](https://github.com/emauton))
- \[Closes \#172\] Check node type is 'remote'. [\#173](https://github.com/inaka/elvis/pull/173) ([jfacorro](https://github.com/jfacorro))
- Fix escript generation with rebar. [\#171](https://github.com/inaka/elvis/pull/171) ([pvalsecc](https://github.com/pvalsecc))
- \[\#100\] Fixed typo. [\#165](https://github.com/inaka/elvis/pull/165) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#100\] Support user defined rules [\#164](https://github.com/inaka/elvis/pull/164) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#160\] Rules config as maps [\#161](https://github.com/inaka/elvis/pull/161) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#148\] Ignore modules configuration to god\_modules rule [\#158](https://github.com/inaka/elvis/pull/158) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#149\] Ignore deps list for no\_deps\_\* rules. [\#157](https://github.com/inaka/elvis/pull/157) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#142\] Removed all files and rules that don't fail from rock/1 return value [\#156](https://github.com/inaka/elvis/pull/156) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#154\] Links to wiki in README [\#155](https://github.com/inaka/elvis/pull/155) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#150\] Use ktn\_code. [\#151](https://github.com/inaka/elvis/pull/151) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#144\] 'install git-hook' command [\#146](https://github.com/inaka/elvis/pull/146) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#143\] Added install target [\#145](https://github.com/inaka/elvis/pull/145) ([jfacorro](https://github.com/jfacorro))

## [0.2.3](https://github.com/inaka/elvis/tree/0.2.3) (2014-10-16)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.2...0.2.3)

**Implemented enhancements:**

- Update version of erlang-github dependency to 0.1.1 [\#140](https://github.com/inaka/elvis/issues/140)

**Closed issues:**

- Elvis should only list Erlang repos [\#137](https://github.com/inaka/elvis/issues/137)

**Merged pull requests:**

- \[Closes \#140\] Updated erlang-github version and rebar.config. [\#141](https://github.com/inaka/elvis/pull/141) ([jfacorro](https://github.com/jfacorro))

## [0.2.2](https://github.com/inaka/elvis/tree/0.2.2) (2014-09-26)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.1...0.2.2)

**Implemented enhancements:**

- Github: For public repos don't add the Services Group [\#128](https://github.com/inaka/elvis/issues/128)
- Use inaka/erlang-github as a dependency  [\#87](https://github.com/inaka/elvis/issues/87)

**Fixed bugs:**

- Wrong detection of macro as function name with ?MODULE [\#133](https://github.com/inaka/elvis/issues/133)
- Reports of lines that include ~p [\#130](https://github.com/inaka/elvis/issues/130)
- Elvis fails when trying to add @elvisinaka as a collaborator… [\#129](https://github.com/inaka/elvis/issues/129)
- Webhook error: 'undef' while applying rule 'used\_ignored\_variable'. [\#122](https://github.com/inaka/elvis/issues/122)
- Elvis webhook is not working [\#120](https://github.com/inaka/elvis/issues/120)

**Closed issues:**

- Add link to service on README.md [\#125](https://github.com/inaka/elvis/issues/125)
- Question: R16 [\#124](https://github.com/inaka/elvis/issues/124)

**Merged pull requests:**

- \[Closes \#87\] Use erlang-github [\#136](https://github.com/inaka/elvis/pull/136) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#133\] Added ?MODULE as an exception for function dynamic calls. [\#135](https://github.com/inaka/elvis/pull/135) ([jfacorro](https://github.com/jfacorro))
- Add missing , in Readme example config. [\#134](https://github.com/inaka/elvis/pull/134) ([sedrik](https://github.com/sedrik))
- \[inaka/elvis-server\#25\] Check team membership GitHub API call. [\#132](https://github.com/inaka/elvis/pull/132) ([jfacorro](https://github.com/jfacorro))
- \[\#130\] Escape tilde before printing result message. [\#131](https://github.com/inaka/elvis/pull/131) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#125\] Added website link and description. [\#126](https://github.com/inaka/elvis/pull/126) ([jfacorro](https://github.com/jfacorro))
- \[\#122\] Added zipper as application so that it is included in release. [\#123](https://github.com/inaka/elvis/pull/123) ([jfacorro](https://github.com/jfacorro))
- \[\#120\] Webhook not working [\#121](https://github.com/inaka/elvis/pull/121) ([jfacorro](https://github.com/jfacorro))

## [0.2.1](https://github.com/inaka/elvis/tree/0.2.1) (2014-09-15)

[Full Changelog](https://github.com/inaka/elvis/compare/0.2.0...0.2.1)

**Implemented enhancements:**

- Improve error handling and reporting [\#107](https://github.com/inaka/elvis/issues/107)

**Fixed bugs:**

- Error when source file contains non ASCII chars [\#113](https://github.com/inaka/elvis/issues/113)
- Unhandled abstract form error on OTP stdlib [\#112](https://github.com/inaka/elvis/issues/112)
- git-hook is not detecting files that are not erlang modules [\#101](https://github.com/inaka/elvis/issues/101)

**Merged pull requests:**

- \[Closes \#107\] Improve error handling and reporting. [\#118](https://github.com/inaka/elvis/pull/118) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#101\] Modified test case, adding non erlang files. [\#116](https://github.com/inaka/elvis/pull/116) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#113\] Added support for unicode characters. [\#115](https://github.com/inaka/elvis/pull/115) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#112\] Added abstract form. [\#114](https://github.com/inaka/elvis/pull/114) ([jfacorro](https://github.com/jfacorro))

## [0.2.0](https://github.com/inaka/elvis/tree/0.2.0) (2014-09-11)

[Full Changelog](https://github.com/inaka/elvis/compare/0.1.1...0.2.0)

**Implemented enhancements:**

- Update README with new configuration format [\#105](https://github.com/inaka/elvis/issues/105)

**Fixed bugs:**

- Elvis tries to parse all files, even non erlang ones [\#108](https://github.com/inaka/elvis/issues/108)

**Closed issues:**

- Don't use old configuration format [\#103](https://github.com/inaka/elvis/issues/103)
- iolists vs. strings vs. binaries [\#25](https://github.com/inaka/elvis/issues/25)
- Records in Specs [\#23](https://github.com/inaka/elvis/issues/23)
- \#state records [\#21](https://github.com/inaka/elvis/issues/21)

**Merged pull requests:**

- \[Fixes \#108\] Avoid parsing non erlang files [\#111](https://github.com/inaka/elvis/pull/111) ([jfacorro](https://github.com/jfacorro))
- \[\#105\] Updated README. Fixed typos. [\#110](https://github.com/inaka/elvis/pull/110) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#103\] Check old Elvis configuration format [\#109](https://github.com/inaka/elvis/pull/109) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#23\] No specs with records. [\#106](https://github.com/inaka/elvis/pull/106) ([jfacorro](https://github.com/jfacorro))
- \[\#21\] State records in OTP modules. [\#104](https://github.com/inaka/elvis/pull/104) ([jfacorro](https://github.com/jfacorro))

## [0.1.1](https://github.com/inaka/elvis/tree/0.1.1) (2014-09-09)

[Full Changelog](https://github.com/inaka/elvis/compare/7c7a0c4cc316f6a3eab6fdbbc5e05c0653be226d...0.1.1)

**Implemented enhancements:**

- make compatible with erlang.mk [\#91](https://github.com/inaka/elvis/issues/91)
- Handle \<\<"ping"\>\> GitHub event [\#53](https://github.com/inaka/elvis/issues/53)
- webhook: read and use the elvis.config from the repository   [\#47](https://github.com/inaka/elvis/issues/47)
- Improve feedback when using Elvis on the command line [\#86](https://github.com/inaka/elvis/issues/86)
- Improve running time by caching file information [\#64](https://github.com/inaka/elvis/issues/64)
-  GitHub API: Use OAuth instead of basic authentication [\#58](https://github.com/inaka/elvis/issues/58)

**Fixed bugs:**

- Elvis/Aleppo can't handle a comment on first line of a module [\#93](https://github.com/inaka/elvis/issues/93)
- used\_ignored\_variable reports unused ignored variables [\#92](https://github.com/inaka/elvis/issues/92)
- ?MODULE should be allowed in the macro\_module\_names rule [\#89](https://github.com/inaka/elvis/issues/89)
- Improve module name default Regex [\#88](https://github.com/inaka/elvis/issues/88)
- operator\_spaces rule complains about missing spaces in comments [\#79](https://github.com/inaka/elvis/issues/79)
- Elvis crashes in `erl\_parse:abstract/2` [\#76](https://github.com/inaka/elvis/issues/76)
- Aleppo library is missing when creating a release [\#70](https://github.com/inaka/elvis/issues/70)
- Access file contents using GitHub API [\#59](https://github.com/inaka/elvis/issues/59)
- operator\_spaces rule complains about missing spaces inside strings [\#42](https://github.com/inaka/elvis/issues/42)
- macro\_names rule complains about "STAGED\_CONTENT\(Path\)" [\#41](https://github.com/inaka/elvis/issues/41)
- Missing link in README [\#39](https://github.com/inaka/elvis/issues/39)
- README: Outdated configuration argument value  [\#34](https://github.com/inaka/elvis/issues/34)
- Error in webhook when elvis.config in repo missing  [\#68](https://github.com/inaka/elvis/issues/68)

**Closed issues:**

- Elvis crashes when running on processone/ejabberd [\#84](https://github.com/inaka/elvis/issues/84)
- Rule idea: pinpoint functions with too many indentation levels [\#82](https://github.com/inaka/elvis/issues/82)
- Elvis can't find header files [\#75](https://github.com/inaka/elvis/issues/75)
- http://elvis.inakalabs.com doesn't advertise character encoding [\#74](https://github.com/inaka/elvis/issues/74)
- Comment position has to be relative to the diff [\#56](https://github.com/inaka/elvis/issues/56)
- if [\#26](https://github.com/inaka/elvis/issues/26)
- Ignored variables [\#22](https://github.com/inaka/elvis/issues/22)
- -callback [\#20](https://github.com/inaka/elvis/issues/20)
- Naming convention [\#19](https://github.com/inaka/elvis/issues/19)
- master in deps [\#18](https://github.com/inaka/elvis/issues/18)
- Dynamic Functions [\#17](https://github.com/inaka/elvis/issues/17)
- God Modules [\#16](https://github.com/inaka/elvis/issues/16)
- Nesting Level [\#14](https://github.com/inaka/elvis/issues/14)
- Code width [\#13](https://github.com/inaka/elvis/issues/13)
- Surround operators and commas with spaces [\#12](https://github.com/inaka/elvis/issues/12)
- Macros in module/function names [\#11](https://github.com/inaka/elvis/issues/11)
- Macro Names [\#10](https://github.com/inaka/elvis/issues/10)
- Rules wiki page [\#8](https://github.com/inaka/elvis/issues/8)
- Build a simple website [\#6](https://github.com/inaka/elvis/issues/6)
- Github Integration [\#5](https://github.com/inaka/elvis/issues/5)
- Commit Integration [\#4](https://github.com/inaka/elvis/issues/4)
- CLI tool [\#3](https://github.com/inaka/elvis/issues/3)
- Erlang console tool [\#2](https://github.com/inaka/elvis/issues/2)
- Initial App Structure [\#1](https://github.com/inaka/elvis/issues/1)

**Merged pull requests:**

- \[Closes \#18\] No deps in revision 'master'. [\#102](https://github.com/inaka/elvis/pull/102) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#86\] Improve command line feedback. [\#99](https://github.com/inaka/elvis/pull/99) ([jfacorro](https://github.com/jfacorro))
- \[Fix \#92\] Fixed bug. [\#98](https://github.com/inaka/elvis/pull/98) ([jfacorro](https://github.com/jfacorro))
- \[\#93\] Added handling for unhandled and unexpected abstract forms. [\#97](https://github.com/inaka/elvis/pull/97) ([jfacorro](https://github.com/jfacorro))
- \[\#91\] Updated erlang.mk version to 1.1 [\#94](https://github.com/inaka/elvis/pull/94) ([jfacorro](https://github.com/jfacorro))
- \[\#89\]\[\#88\] MODULE as a valid module call part. '\_SUITE' as a module name suffix. [\#90](https://github.com/inaka/elvis/pull/90) ([jfacorro](https://github.com/jfacorro))
- \[\#84\] Fixed bugs. Implemented missing nodes. [\#85](https://github.com/inaka/elvis/pull/85) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#79\] Ignore op\_space if node is not found on parse tree. [\#80](https://github.com/inaka/elvis/pull/80) ([jfacorro](https://github.com/jfacorro))
- \[\#76\] Added record definition clauses for to\_map function. [\#78](https://github.com/inaka/elvis/pull/78) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#75\]\[Fixes \#76\] Use config src\_dirs to look for header files. [\#77](https://github.com/inaka/elvis/pull/77) ([jfacorro](https://github.com/jfacorro))
- \[inaka/elvis-server\#19\] Add user to Services team. [\#73](https://github.com/inaka/elvis/pull/73) ([jfacorro](https://github.com/jfacorro))
- \[inaka/elvis-server\#17\] List all the repositories for which the user has admin permission. [\#72](https://github.com/inaka/elvis/pull/72) ([jfacorro](https://github.com/jfacorro))
- \[\#70\] Added aleppo as applications to the .app file. [\#71](https://github.com/inaka/elvis/pull/71) ([jfacorro](https://github.com/jfacorro))
- \[\#68\] Fixed bug when elvis.config is missing from the repo. [\#69](https://github.com/inaka/elvis/pull/69) ([jfacorro](https://github.com/jfacorro))
- \[inaka/elvis-server\#8\] Added GitHub API calls for Elvis's website  [\#67](https://github.com/inaka/elvis/pull/67) ([jfacorro](https://github.com/jfacorro))
- \[\#58\] Added support for OAuth. [\#66](https://github.com/inaka/elvis/pull/66) ([jfacorro](https://github.com/jfacorro))
- \[\#64\] Improved performance by processing parse tree only once. [\#65](https://github.com/inaka/elvis/pull/65) ([jfacorro](https://github.com/jfacorro))
- \[\#47\] Use the repository's elvis.config file [\#63](https://github.com/inaka/elvis/pull/63) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#42\] Strings, atoms and binaries are new ignored. [\#62](https://github.com/inaka/elvis/pull/62) ([jfacorro](https://github.com/jfacorro))
- \[\#41\] Fixed bug in macro\_names rule [\#61](https://github.com/inaka/elvis/pull/61) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#59\] Using API to get files' content [\#60](https://github.com/inaka/elvis/pull/60) ([jfacorro](https://github.com/jfacorro))
- \[\#56\] Relative position for PR comments [\#57](https://github.com/inaka/elvis/pull/57) ([jfacorro](https://github.com/jfacorro))
- \[\#53\] Added handling for GitHub's ping event [\#55](https://github.com/inaka/elvis/pull/55) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#19\] Implemented 'module naming convention' rule. [\#52](https://github.com/inaka/elvis/pull/52) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#20\] Implemented 'no behavio\(u\)r\_info' rule. [\#51](https://github.com/inaka/elvis/pull/51) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#17\] Implemented 'used ignored variable' rule. [\#50](https://github.com/inaka/elvis/pull/50) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#17\] Implemented invalid dynamic calls rule. [\#49](https://github.com/inaka/elvis/pull/49) ([jfacorro](https://github.com/jfacorro))
- \[\#26\] Implemented 'no if expression' rule. [\#48](https://github.com/inaka/elvis/pull/48) ([jfacorro](https://github.com/jfacorro))
- \[\#16\] Implemented god modules rule. [\#46](https://github.com/inaka/elvis/pull/46) ([jfacorro](https://github.com/jfacorro))
- \[\#14\] Modified AST node generation for 'case' and list comprehension [\#45](https://github.com/inaka/elvis/pull/45) ([jfacorro](https://github.com/jfacorro))
- \[\#14\] Implement nesting level rule [\#43](https://github.com/inaka/elvis/pull/43) ([jfacorro](https://github.com/jfacorro))
- \[\#5\] GitHub Integration: errors and bugs fixes [\#40](https://github.com/inaka/elvis/pull/40) ([jfacorro](https://github.com/jfacorro))
- \[\#5\] Implemented GitHub Integration [\#37](https://github.com/inaka/elvis/pull/37) ([jfacorro](https://github.com/jfacorro))
- Fixes \#12: Operator Spaces [\#36](https://github.com/inaka/elvis/pull/36) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fixes \#11: Macros in module/function names [\#35](https://github.com/inaka/elvis/pull/35) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#4\] Added git-hook command [\#33](https://github.com/inaka/elvis/pull/33) ([jfacorro](https://github.com/jfacorro))
- \[Fixes \#10\] Macro Names [\#32](https://github.com/inaka/elvis/pull/32) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#3\] Implemented a command line interface for Elvis. [\#31](https://github.com/inaka/elvis/pull/31) ([jfacorro](https://github.com/jfacorro))
- \[\#2\] Implemented elvis for console usage [\#9](https://github.com/inaka/elvis/pull/9) ([jfacorro](https://github.com/jfacorro))
- \[\#1\] Rebar and erlang.mk configuration files and other application structure files. [\#7](https://github.com/inaka/elvis/pull/7) ([jfacorro](https://github.com/jfacorro))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
