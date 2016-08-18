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

### Example

These are the commands I had to run to add a new dependency. Note that none of
these directories existed prior to running this command:
"vendor/github.com/go-sql-driver/mysql":

```
git remote add mysql git@github.com:go-sql-driver/mysql.git
git fetch mysql
git read-tree --prefix=vendor/github.com/go-sql-driver/mysql -u mysql/master
```

To update an existing dependency I think all that needs to be done is:

```
git fetch mysql
git merge -s subtree --squash mysql/master
# If the above command doesn't work (git can't figure out where to put the
# code) then it seems this should do the trick.
git merge -X subtree=vendor/github.com/go-sql-driver/mysql --squash mysql/master
```

It looks like there are also ways to push local changes to a dependency to its
github repository but I'll learn those when needed.

Deleting a Branch
-----------------

```
git -d branch-to-delete
```

Cherry Picking
--------------

http://think-like-a-git.net/sections/rebase-from-the-ground-up/cherry-picking-explained.html

Weird Issue
-----------

So Zach had a weird issue that I'd like to figure out one of these days when I
have the time. I believe the order of events were:

1. Made a branch+commit+PR changing files A and B. This PR was merged.
2. The PR had an error in it so he hit the "Revert" button to revert that PR
   and that revert was merged.
3. He made another change on the same branch and when he made a PR out of that
   change, he expected the changes in 1 to appear but they did not. I believe
   the issue is that he should rebase off the base branch, cherry-pick the
   commit, and then push again. But I was just wondering what is going on in
   git's mind. How does a git PR know what files have changed?
