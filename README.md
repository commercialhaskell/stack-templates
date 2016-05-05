A repository for templates used by `stack new`.
See: http://haskellstack.org

[![Build Status](https://travis-ci.org/commercialhaskell/stack-templates.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack-templates)

## Introduction

Project templates are written in hsfiles format, using [mustache](https://mustache.github.io/mustache.1.html). Each
file is specified with `START_FILE`, like this:

```
{-# START_FILE {{name}}.cabal #-}
name:                {{name}}
version:             0.1.0.0
...
```

Parameters to the template are written `{{foo}}`. They are provided by
users via their `~/.stack/config.yaml` file, like this:

``` yaml
templates:
  params:
    author-email: chrisdone@gmail.com
    author-name: Chris Done
    copyright: 2016 Chris Done
    github-username: chrisdone
    category: Development
```

When the user runs `stack new myproject yourtemplate` and they do not
have the parameters provided in, it will warn the user that such
parameters were missing, like this:

```
$ stack new foo new-template
Downloading template "new-template" to create project "foo" in foo/ ...
The following parameters were needed by the template but not provided: author-email, author-name
You can provide them in /home/chris/.stack/stack.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
Or you can pass each one as parameters like this:
stack new foo new-template -p "author-email:value" -p "author-name:value"
```

The output of the template will yield a blank space where your
parameter was. If you want to provide default values for your template
parameters, use this Mustache syntax:

```
author:              {{author-name}}{{^author-name}}Author name here{{/author-name}}
```

## `template-info.yaml`

When contributing a new template, please remember to add a corresponding entry to `template-info.yaml`. Additional descriptive information for the template may be provided, but is not required.
