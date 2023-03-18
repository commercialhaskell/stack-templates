# Stack Templates

The `stack new` command will create a new project based on a project template.
Templates can be located on the local filesystem; Github, GitLab or
Bitbucket repositories; or arbitrary URLs.

Stack allows any GitHub, GitLab or Bitbucket repository named
`stack-templates` to provide project templates for Stack. For example, a
template file at `username/stack-templates/my-template.hsfiles` on GitHub can be
identified as `username/my-template` when using `stack new`. The relevant
service can be specified by prefixing `github:` (the default), `gitlab:` or
`bitbucket:`.

For more information, please see the user guide:

https://docs.haskellstack.org/en/stable/templates_command/

There are many project templates available at
`commercialhaskell/stack-templates` on GitHub (the default repository) or other
locations. Some simple examples:

    stack new myproj # uses the default template (new-template)
    stack new myproj2 rio # uses the rio template at the default repository
    stack new website yesodweb/sqlite # Yesod server with SQLite DB

For more information and other project templates, please see the
`stack-templates` Wiki:

https://github.com/commercialhaskell/stack-templates/wiki

Please feel free to add information about your own project templates to that
Wiki for discoverability.

Want to improve this text? Send us a pull request!

https://github.com/commercialhaskell/stack-templates/edit/master/STACK_HELP.md
