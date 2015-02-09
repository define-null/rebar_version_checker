# rebar_version_checker #

A rebar plugin which gives an apportunty to check dependencies against version constraints.

## Installation ##

Add the following to your top-level rebar.config:

    %% Plugin dependency
    {deps, [
    	{rebar_version_checker, ".*",
         {git, "https://github.com/define-null/rebar_version_checker.git", {branch, "master"}}}
    ]}.

    %% Plugin usage
    {plugins, [rebar_version_checker]}.

## How it works ##

The command 'vsn-check' must be run from a rebar project directory in which `get-deps` has already been run. Plugin search for section `vsn_check` in all rebar.config's and check each dependency mentioned in this section against the concrete constraints of current config. Make sure that versions of checked dependencies and version in constraints must satisfy semantic versioning 2.0.0. (http://semver.org/spec/v2.0.0.html).

Due to the fact that rebar's git verstioning approach adds 'v' letter in the front, plugin ignores 'v' letter at assume that the remaining part correspond to schema versioning.

All schema versioning is done with the help of https://github.com/nox/mouture

## Example

Rebar config example:

     {deps, [
             {lager,   ".*", {git, "git://github.com/basho/lager",              {tag, "2.0.3"}}},
             {cowboy,  ".*", {git, "git://github.com/extend/cowboy.git",        {tag, "0.9.0"}}},
             {rebar_version_checker, ".*",
             {git, "https://github.com/define-null/rebar_version_checker.git", {branch, "master"}}}
            ]}.

     {plugins, [rebar_version_checker]}.

     {vsn_check, [
                  {lager, ["> 2.0.0", "<= 2.3.3"]},
                  {cowboy,[">= 1.0.0"]}
                 ]}.

Run vsn-check:

    me@localdomain:~/Dev/example] $ rebar vsn-check
    ==> lager (vsn-check)
    ==> cowlib (vsn-check)
    ==> ranch (vsn-check)
    ==> cowboy (vsn-check)
    App cowboy "0.9.0" is not matching constraint ">= 1.0.0" in "/Users/me/Dev/example"

