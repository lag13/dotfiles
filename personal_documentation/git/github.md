Github
======

API
---

https://developer.github.com/v3/

Returns the contents of a particular file. I believe there are other ways but
this is one:

   curl -H "Accept: application/vnd.github.v3.raw" https://api.github.com/repos/lag13/dotfiles/contents/README.md
