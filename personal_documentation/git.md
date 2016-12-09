Git
===

https://progit.org/
https://thoughtbot.com/upcase/mastering-git
http://ohshitgit.com/

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

Break Up Recent Commit
----------------------

http://stackoverflow.com/questions/6217156/break-a-previous-commit-into-multiple-commits

```
# Undoes (sort of) the most recent commit
git reset HEAD~
```

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

Git Basics
----------

From: https://vimeo.com/17118008

Slides:
http://www.erlang-factory.com/upload/presentations/244/ErlangFactroySFBay2010-TomPreston-Werner.pdf

              +-------+
              | Index |
              +-------+
              
              
+---------+
| Commits |
+---------+
              
              +-------------------+
              | Working Directory |
              +-------------------+

blob = how git stores a file

tree = how git stores a directory structure

reference = a way to refer to a commit. Examples are: the commit hash, part of
the commit hash, a branch name, HEAD, HEAD^^.

working directory = the files you have locally that are tracked by git. I
beleieve this is also called the working tree although it seems there is a
command called worktree so maybe I'm wrong:
https://git-scm.com/docs/git-worktree

index = when you do a `git add` the files get put into the index. I beleieve
"staging" is another word for "index". A commit is made by taking a snapshot
of the index. It looks like the index is literally just a file: .git/index.
Actually I suppose it is represented by this file? Man I wonder if git status
really just does `cat .git/index` that would be cool.

commit = a snapshot of the index. Every commit is really a snapshot of your
entire project. Its not a diff, it is literally a copy of the index at the
time of the commit. The commit is identified by a 40 character sha and has a
message associated with it.

when you do a `git diff` it compares the working directory to the contents of
the index. `git diff --cached` or `git diff --staged` will show the difference
between the contents of the staged changes (index) and the last commit.

`git add -p` lets you commit "hunks" i.e you individually commit each change
you made to the files.

Git workflow:

1. Make changes to the working directory
2. Stage those changes to the index
3. Commit the current state of the index

Lets say you modifiy a file and add it to the index. Now the index is really
the previous index + this change so the index still contains ALL the files.

Every commit has zero or more parent commits. In this diagram C0 is the parent
of C1 which is the parent of C2:

C0 <--- C1 <---- C2

Branches are just pointers to commits. They are literally just a file where
the name of the file is the branch name and the contents are the sha of the
commit they point to. The branches seem to be located in: .git/refs/heads/.
There is really no more to it than that. You can list branches with `git
branch` and `git branch -v` will also show the commits they point to. As you
commit, the branch you are on moves with you. Doing `git branch branch-name`
creates the branch branch-name but does not switch to it.

Its easiest to think of the working directory as corresponding to a branch. By
"corresponding" I mean that the working directory uses that branch as a "base"
to see if there have been changes or not. I suppose more general you should
think of the working directory as corresponding to a particular commit.

When you switch branches the index changes to reflect the commit pointed to by
that branch. If there were staged changes (i.e the index is the commit +
changes) then when switching it will try to bring the "+ changes" part and
apply them to the new branch being switched to.

`git stash [save]` makes a temporary commit out of what is in the working
directory and index and puts it somewhere you don't have to worry about it
effectively reverting you back to the last commit. You can do `git stash
apply` or `git stash pop` to get back those changes. I think the use case is
when you have some changes to your branch, want to switch to a new one, want
to save those changes, but don't want a real commit (because you are not
finished). So you stash and switch to another branch. When you want to keep
working you do apply/pop. BUG???? I'm not sure if its a bug but I notice that
if I stage a file, do `git stash save`, then do `git stash pop` that staged
file is now unstaged. It looks like things you've stashed with git are global
entities, they do not correspond to a particular branch or anything like that.

HEAD (also "head" or @) is a dynamic reference for the current checkout.
Normally HEAD points to a branch which then points to a commit. In this
scenario HEAD is considered "attached" to a branch. When HEAD points directly
to a commit it is considered "detached". Doing `git symbolic-ref HEAD` returns
the branch that HEAD is pointing to. Doing `git rev-parse refs/heads/master`
returns the commit pointed to by the master branch. Doing `git rev-parse HEAD`
returns the commit pointed to by HEAD. HEAD^ referrs to the parent of this
commit (i.e the commit that came before). If a commit has two commits the
other commit will be HEAD^2. HEAD^^ referrs to the commit before HEAD^ etc...
HEAD~2 is another name for HEAD^^. `git reflog` shows previous values of HEAD.

I think the '.' in `git add .` means add the current directory so it will add
everything if you are in the root of your git repository. Can't believe I did
not see that before.

There are different types of merges:

1. Fast forward - The branch being merged into the current branch is a direct
   decendent of the current branch so the current branch can quite literally
   be moved forward to point to the newest commit. IS THIS REALLY TRUE?? SO IF
   I CLICK MERGE ON GITHUB AND THE THING BEING MERGED IS JUST A FASTFORWARD OF
   MASTER IT WILL MOVE MASTER FORWARD?
2. Recursive - uses a 3 way merge strategy. The three parts are: The commit A
   before the branch split off, the commit(s) B that happened on the master
   branch since A, the commit(s) C that happened on a different branch. Then a
   new commit D is made which merges B and C together. This new "merge" commit
   D has two parents B and C.

`git fetch remote` fetches all commits from the given remotes and updates the
remote branches. Your local branches are unchanged. Doing `git diff head
origin/master` will compare the current commit to the origin/master branch.
The remotes are stored in .git/refs/remotes/. When you do `git push branch
remote` 'branch' will be created as a remote branch if it did not exist
already.

DO YOU NEED TO USE GITHUB TO COLLABORATE WITH GIT? IT FEELS LIKE YOU SHOULD
NOT NEED TO DO THIS BUT HOW ELSE WOULD YOU?

More Git
--------

https://thoughtbot.com/upcase/mastering-git

### First Video

You can think of commits as "checkpoints" or "save states" in your code. You
can relate this to a game.

Apparently you rebase to rewrite history you actually don't lose the old
commits, git basically just constructs a new tree of commits and points you
there but the old tree still exists.

There is a `-u` option for `git stash` which will make it save untracked files
as well. Perhaps I should use this more often? When I take random notes I can
put them in a `personal-notes` folder or something and have .gitignore ignore
that directory. And thats what I just did.

WHAT HAPPENS IF YOU DO MULTIPLE git stash's? WILL THEY STEP ON EACHOTHER'S
TOES?

`git reflog` command will show every action you have taken on the repository
(commits, branch switching, merges, etc...). So it can be useful if something
goes wrong and you need to figure out why something went wrong. `git reflog
show ref` will show all the change relating to the reference `ref`.

Hooks
-----

- https://git-scm.com/book/en/v2/Customizing-Git-An-Example-Git-Enforced-Policy
- https://www.digitalocean.com/community/tutorials/how-to-use-git-hooks-to-automate-development-and-deployment-tasks

Here is a simple script I wrote which prevents me from committing a specific
file. The file is .git/hooks/pre-commit:

```
#!/usr/bin/env ruby

# Makes it impossible to commit the "devutil/.env" file.
cannot_commit = "devutil/.env"
files_modified = `git diff-index --cached --name-only HEAD`.split("\n")
files_modified.each do |file|
  if file == cannot_commit
    puts "[POLICY] You cannot commit the #{cannot_commit} file. If this is a mistake, edit the .git/hooks/pre-commit file."
    exit 1
  end
end
```
