Github
======

API
---

https://developer.github.com/v3/

Returns the contents of a particular file. I believe there are other ways but
this is one:

   curl -H "Accept: application/vnd.github.v3.raw" https://api.github.com/repos/lag13/dotfiles/contents/README.md

Faster Pull Requests
--------------------

When making pull requests for release-4.0 for the luceo repository,
the website sometimes slows down to a crawl and it actually takes a
while until I can make the PR. So instead of creating the PR through
the github interface, use this link structure to do it:
https://github.com/cbdr/luceo/compare/release-4.0...add-test-dbs-to-repo40?expand=1
