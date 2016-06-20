Git
===

Deleting a Tag
--------------

https://nathanhoad.net/how-to-delete-a-remote-git-tag

git tag -d my_tag
git push origin :refs/tags/my_tag

Submodules
----------

### Removing a submodule

http://stackoverflow.com/questions/29850029/what-is-the-current-way-to-remove-a-git-submodule

```
git submodule deinit asubmodule    
git rm asubmodule
rm -rf .git/modules/asubmodule
```

Subtrees
--------

https://medium.com/@porteneuve/mastering-git-subtrees-943d29a798ec#.e7m0v8t3j
http://blogs.atlassian.com/2013/05/alternatives-to-git-submodule-git-subtree/

